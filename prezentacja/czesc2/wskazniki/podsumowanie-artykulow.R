# OPIS
# uporządkowanie artykułów
# stworzenie jednej tabelki dla wszystkich artykułów
# na niej będziemy robić prosty text0mining i word cloud

library(stringi)
library(dplyr)

############################################################################ 
# funkcje potrzebne w funkcji zrobListeRamekPortalKandydat
############################################################################ 

pobierzDane <- function(portal, x){
   kod1 <- c("NaTematGlowna", "NaTematWybory", "OnetGlowna", "OnetWybory", "wPolityceGlowna")
   kod2 <- c("interia", "tvn24", "wp", "wsieci")
   if(portal %in% kod1) dane <- read.csv(x)
   if(portal %in% kod2) dane <- read.csv(x, encoding = "UTF-8")
   return(dane)
}

znajdzKolumny <- function(portal, dane){
   if(portal %in% c("NaTematGlowna", "NaTematWybory", "OnetWybory")){
      tytul <- dane[, 1]
      artykul <- dane[, 3]
   }
   if(portal %in% c("OnetGlowna")){
      tytul <- dane[, 1]
      artykul <- dane[, 2]
   }
   if(portal %in% c("interia", "wp", "wsieci")){
      tytul <- dane[, 2]
      artykul <- dane[, 3]
   }
   if(portal == "tvn24"){
      tytul <- dane[, 3]
      artykul <- dane[, 4]
   }
   if(portal == "wPolityceGlowna"){
      tytul <- dane[, 1]
      artykul <- dane[, 5]
   }
   return(list(tytul, artykul))
}

nadajDate <- function(nazwa){
   czy.z.godzina <- stri_detect_fixed(nazwa, "@")
   if(czy.z.godzina == TRUE){
      data <-  as.character(strptime(nazwa, "%F@%H-%M"))
   } else {
      data <- as.character(strptime(nazwa, "%F"))
      data <- paste(data, "00:00:00")
   }
   return(data)
}


############################################################################ 
# dla danego folderu typu np. interia/komorowski robimy ramke danych
# z plików, które nie były jeszcze dołączone
############################################################################ 

zrobListeRamekPortalKandydat <- function(pliki){
   regexy <- c(
      "(Komorowski(\\p{L})*)|((B|b)ronkobus(\\p{L})*)",
      "Marian(\\p{L})* Kowalsk(\\p{L})*",
      "(Dud(\\p{L})*)|((D|d)udabus(\\p{L})*)",
      "Paliko(\\p{L})*",
      "Jarubas(\\p{L})*",
      "Ogórek",
      "Korwin(\\p{L})*",
      "Ann(\\p{L})+ Grodzk(\\p{L})*",
      "Jac(e)*(\\p{L})* Wilk(\\p{L})*",
      "Grzegorz(\\p{L})* Braun(\\p{L})*",
      "Kukiz(\\p{L})*",
      "Tanajn(\\p{L})*",
      "prezydent(\\p{L})*",
      "prezydenck(\\p{L})*",
      "andrzej(\\p{L})*",
      "bronis(\\p{L})*",
      "kaczyński(\\p{L})*",
      "kandyda(\\p{L})*",
      "wybor(\\p{L})*",
      "kampani(\\p{L})*",
      "janusz(\\p{L})*",
      "pols(\\p{L})*",
      "pola(k|c)(\\p{L})*",
      "adam(\\p{L})*",
      "podpis(\\p{L})*"
   )
   
   names(regexy) <- c("komorowski", "kowalski", "duda", "palikot", "jarubas", "ogorek",
                      "korwin", "grodzka", "wilk", "braun", "kukiz", "tanajno", 
                      "prezydent", "prezydenckie", "andrzej", "bronislaw", "kaczynski", "kandydat",
                      "wybory", "kampania", "janusz", "polska", "polacy", "adam",
                      "podpisy")
   
   lista.ramek.portal.kandydat <- lapply(pliki, function(x){
      
      portal <- stri_extract_first_regex(x, "(?<=_)[^_]+")
      kandydat <- stri_extract_last_regex(x, "(?<=_)[^(\\.|_)]+")
      
      dane <- pobierzDane(portal, x)
      n <- nrow(dane)
      kolumny <- znajdzKolumny(portal, dane)
      
      #data artykułu (przyjmujemy dla ujednolicenia, że data artykułu to data jego pobrania)
      kiedy <- nadajDate(x)
      data <- rep(kiedy, n)
      #nr artykułu (w danym pliku, czyli w danym dniu i ewentualnie o danej godzinie)
      nr <- c(1:n)
      #tesc artykulu
      artykul <- as.character(kolumny[[2]])
      #zamieniamy odmiany nazwisk na nazwisko w formie podstawowej
      artykul <- oczyscArtykul(artykul, regexy, names(regexy))
      
      #ramka danych dla pojedynczego pliku
      ramka.plik <- data.frame(portal, kandydat, data, nr, artykul, stringsAsFactors=FALSE)
      return(ramka.plik)
   })
   return(lista.ramek.portal.kandydat)
}

oczyscArtykul <- function(art, regexy, zamiana){
   regexy.lepsze <- stri_paste("(?i)(\\p{L})*", regexy, sep="")
   names(regexy.lepsze) <- zamiana
   for (i in seq_along(regexy.lepsze)){
      art <- stri_replace_all_regex(art, regexy.lepsze[i], names(regexy.lepsze)[i])
   }
   return(art)
}

zrobJednaRamkePortalKandydat <- function(dataFrameList){
   n <- length(dataFrameList)
   if (n==1){
      return(dataFrameList[[1]])
   } else {
      tmpDataFrame <- dataFrameList[[1]]
      for (i in 2:n){
         bindDataFrames <- rbind(tmpDataFrame, dataFrameList[[i]])
         tmpDataFrame <- bindDataFrames
      }
      return(tmpDataFrame)
   }
}

usunStopWords <- function(tresc){
   sw <- readLines("polish-stopwords.txt", encoding="UTF-8")
   corpus <- Corpus(VectorSource(tresc))
   corpus <- tm_map(corpus, removeWords, sw)
   #dodatkowo usuwam jeszcze kilka innych słów (wcześniejsza analiza tekstu)
   corpus <- tm_map(corpus, removeWords, 
                    c("ani", "myślę", "mówiła", "choć", "wraz", "bądź", 
                      "mówią", "robi", "nikt", "wie", "mówił", "powiedział",
                      "chce", "swojego", "swój", "swoj", "swojej", "temu", 
                      "będziemy", "mowi", "swoich", "wśród", "swoim", "mieć",
                      "mówi", "to", "ze", "fot", "miał", "dodał"))
   dataframe <- data.frame(text=unlist(sapply(corpus, `[`, "content")), 
                         stringsAsFactors=F)
   return(dataframe)
}


#ustawianie ścieżki do folderu z danymi
setwd("D:/GoogleDrive/Studia/SemestrVIII/R-i-Big-Data/Wybory/dane")
glowna <- getwd()

#szukamy scieżek do folderów portal/kandydat
foldery.all <- list.dirs(full.names = FALSE)
ktore.nietwitter <- !stri_detect_regex(foldery.all, "twitter")
ktore.niefacebook <- !stri_detect_regex(foldery.all, "(?i)facebook")
ktore.nieglowne <- stri_detect_regex(foldery.all, "/")
#lista folderów dla poszczególnych kandydatów z danych portali 
#(z wyłączeniem twittera i facebooka)
foldery <- foldery.all[ktore.nietwitter & ktore.niefacebook & ktore.nieglowne]

a <- list()
if(!file.exists("artykuly.txt")){
   
   for (i in seq_along(foldery)){
      sciezka <- foldery[i]
      setwd(file.path(glowna, sciezka))
      pliki.all <- list.files()
      if(length(pliki.all) > 0) {
         a[[i]] <- zrobJednaRamkePortalKandydat(zrobListeRamekPortalKandydat(pliki.all))
      }
      setwd(glowna)
   }
   b <- zrobJednaRamkePortalKandydat(a)
   czy.duplikaty <- duplicated(b[,5])
   b <- b[!czy.duplikaty, ]
   tresc <- usunStopWords(b[,5])
   b[,5] <- tresc 
   write.table(b, "artykuly.txt", row.names=FALSE, sep=",")
   
} else {
   
   #wczytujemy plik artykuly, do którego będziemy dołączać kolejne wiersze
   podsumowanie <- read.csv("artykuly.txt")
   for (i in seq_along(foldery)){
      sciezka <- foldery[i]
      print(sciezka)
      setwd(file.path(glowna, sciezka))
      
      portal.tmp <- stri_extract_first_regex(sciezka, "[^/]+")
      kandydat.tmp <- stri_extract_last_regex(sciezka, "[^/]+")
      
      #niestety dla jednego portalu mamy małą rozbieżność nazw
      if(portal.tmp == "wPolityce") portal.tmp <- "wPolityceGlowna"
      
      #maksymalna data dla portalu i kandydata w pliku podsumowanie.txt
      portalKandydat <- podsumowanie %>%
         filter(portal == portal.tmp, kandydat == kandydat.tmp) 
      if(nrow(portalKandydat) == 0) next
      
      max.data <- portalKandydat %>%
         summarise(max.data = max(strftime(data, "%F %T")))
      max.data <- as.character(max.data)
      max.data <- strptime(max.data, "%F %T")
      
      #szukamy plików z datą młodszą niż znaleziona w pliku
      #i tylko dane z tych plików dołączamy do obecnych danych
      pliki.all <- list.files()
      n <- length(pliki.all)
      if(n == 0) next
      
      #sprawdzamy czy pliki są z godziną czy bez (sama data)
      ost <- pliki.all[n]
      czy.godzina <- stri_detect_regex(ost, "@")
      if(czy.godzina == TRUE){
         data.pliku <- strptime(ost, "%F@%H-%M")
         indeksy.nowe <- logical(n)
         while(data.pliku - max.data > 0){
            indeksy.nowe[n] <- TRUE
            n <- n - 1
            if (n == 0) next
            plik.tmp <- pliki.all[n]
            data.pliku <- strptime(plik.tmp, "%F@%H-%M")
         }
      } else {
         data.pliku <- strptime(ost, "%F")
         indeksy.nowe <- logical(n)
         while(data.pliku - max.data > 0){
            indeksy.nowe[n] <- TRUE
            n <- n - 1
            if (n == 0) next
            plik.tmp <- pliki.all[n]
            data.pliku <- strptime(plik.tmp, "%F")
         }
      }
      
      #nazwy nowych plikow 
      pliki <- pliki.all[indeksy.nowe]
      
      if(length(pliki) > 0) {
         a[[i]] <- zrobJednaRamkePortalKandydat(zrobListeRamekPortalKandydat(pliki))
      }
      setwd(glowna)
   }
   if(length(a) > 0){
      b <- zrobJednaRamkePortalKandydat(a)
      czy.duplikaty <- duplicated(b[,5])
      b <- b[!czy.duplikaty, ]
      tresc <- usunStopWords(b[,5])
      b[,5] <- tresc 
      write.table(b, "artykuly.txt", row.names=FALSE, col.names = FALSE, 
                  append=TRUE, sep=",") 
   }   
}

#jeżeli dopisujemy dane do już istniejących to musimy porównać czy nowo dodane są różne 
#od już istaniejących

aktualne <- read.csv("artykuly.txt")
czy.duplikaty <- duplicated(aktualne[, 5])
aktualne <- aktualne[!czy.duplikaty, ]
write.table(aktualne, "artykuly.txt", row.names=FALSE, sep=",")
