# OPIS
# wyciągnięcie podstawowych statystyk z dotychczas zebranych artykułów
# i skondencowanie takich danych do tablei

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
      typ <- rep("brak", nrow(dane))
   }
   if(portal %in% c("OnetGlowna")){
      tytul <- dane[, 1]
      artykul <- dane[, 2]
      typ <- dane[, 4]
   }
   if(portal %in% c("interia", "wp", "wsieci")){
      tytul <- dane[, 2]
      artykul <- dane[, 3]
      typ <- dane[, 1]
   }
   if(portal == "tvn24"){
      tytul <- dane[, 3]
      artykul <- dane[, 4]
      typ <- dane[, 1]
   }
   if(portal == "wPolityceGlowna"){
      tytul <- dane[, 1]
      artykul <- dane[, 5]
      typ <- dane[, 4]
   }
   return(list(tytul, artykul, typ))
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

nadajWage <- function(portal, typ){
   n <- length(typ)
   waga <- numeric(n)
   if(typ[1]=="brak"){ #OnetWybory, #NaTematGlowna, #NaTematWybory
      waga <- rep(2, n) #taka sama wszedzie 
   }
   if(portal=="interia"){
      waga[typ=="duży news"] = 4
      waga[typ=="obrazki duze"] = 3
      waga[typ=="newsy"] = 2
   }
   if(portal=="OnetGlowna"){
      waga[typ=="duze"] = 4
      waga[typ=="srednie"] = 3
      waga[typ=="male"] = 2
   }
   if(portal=="tvn24"){
      waga[typ=="newsy"] = 2
      waga[typ=="obrazki"] = 3
      waga[typ=="maintopic"] = 4
   }
   if(portal=="wp"){
      waga[typ=="main topic"] = 4
      waga[typ=="news"] = 2
      waga[typ=="obrazki"] = 3
      waga[typ=="male obrazki"] = 1
   }
   if(portal=="wPolityceGlowna"){
      waga[typ=="top"] = 4
      waga[typ=="top2"] = 4
      waga[typ=="duze"] = 3
      waga[typ=="srednie"] = 2
      waga[typ=="male"] = 2
      waga[typ=="mini"] = 1
   }
   if(portal=="wsieci"){
      waga[typ=="glowna"] = 4
      waga[typ=="obrazki"] = 3
   }
   return(waga)
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
      "Tanajn(\\p{L})*"
   )
   
   names(regexy) <- c("komorowski", "kowalski", "duda", "palikot", "jarubas", "ogorek",
                      "korwin", "grodzka", "wilk", "braun", "kukiz", "tanajno")
   
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
      #czy nazwisko kandydata pojawia sie w tytule artykulu
      regex <- paste("(?i)", regexy[kandydat], sep="")
      czy.w.tytule <- stri_detect_regex(kolumny[[1]], regex)
      #długość artykułu
      tresc <- as.character(kolumny[[2]])
      dlugosc.art <- nchar(tresc)
      #ile razy w artykule pojawia się regex kandydata
      ile.razy <- stri_count_regex(tresc, regex)
      #częstość występowania regexa w artykule
      ile.razy.wzg <- ile.razy/dlugosc.art
      #waga artykulu
      typ <- kolumny[[3]]
      waga <- nadajWage(portal, typ)
      
      #ramka danych dla pojedynczego pliku
      ramka.plik <- data.frame(portal, kandydat, data, nr, czy.w.tytule, dlugosc.art, ile.razy, 
                               ile.razy.wzg, waga, stringsAsFactors=FALSE)
      return(ramka.plik)
   })
   return(lista.ramek.portal.kandydat)
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


############################################################################ 
# odpalanie funkcji powyżej 
############################################################################ 

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
if(!file.exists("podsumowanie.txt")){
   
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
   write.table(b, "podsumowanie.txt", row.names=FALSE, sep=",")
   
} else {
   
   #wczytujemy plik podsumowanie, do którego będziemy dołączać kolejne wiersze
   podsumowanie <- read.csv("podsumowanie.txt")
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
      write.table(b, "podsumowanie.txt", row.names=FALSE, col.names = FALSE, 
                  append=TRUE, sep=",") 
   }   
}

