#skrypt do ściągania artykułow z natemat - strona główna
#propozycja automatyzacji - uruchamianie skryptu codziennie

library(rvest)
library(stringi)
library(tm)

urlTematGlowna <- "http://natemat.pl/"
htmlTematGlowna <- html(urlTematGlowna)
nodes <- html_nodes(htmlTematGlowna, ".without-body-header .hp-content-image+ h2 a")

#wyciagam linki do artykułów
urlArt <- html_attr(nodes, "href")
#indykator czy artykuł jest z natemat czy nie
indNaTemat <- stri_detect_regex(urlArt, ".+natemat.+")
#biorę tylko te, które są z natemat (bo chcę żeby artykuły miały tę samą strukturę)
urlArt <- urlArt[indNaTemat]

#wyciągam tytul
tytul <- html_text(nodes)[indNaTemat]

#wyciagam tylko te artykuly, w ktorego tytule pojawiło się nazwisko kandydatów
kandydat <- c(
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
   "Kukiz(\\p{L})*"
)

n <- length(tytul)
czyWybory <- logical()
for (i in 1:n){
   tyt <- tytul[i]
   czyNazwisko <- sapply(kandydat, function(x){
      stri_detect_regex(tyt, x)
   })
   czyWybory[i] <- any(czyNazwisko)
}

#uaktualniam listę 
urlArt <- urlArt[czyWybory]
tytul <- tytul[czyWybory]

#wyciągam date
dataRaw <- sapply(urlArt, function(x){
   nodesData <- html_nodes(html(x), ".date")
   dataa <- html_attr(nodesData, "title")
   dataa <- stri_replace_all_fixed(dataa, "T", " ")
   dataa <- stri_replace_all_regex(dataa, "[+-][0-9]{2}:[0-9]{2}", "")
   #dataa <- as.character(as.POSIXct(strptime(dataa, "%F %T")))
   #dataa <- as.POSIXct(dataa)
   #dataa <- as.character(as.POSIXct(dataa, "%F %T"))
   
   return(dataa)
})
names(dataRaw) <- NULL
data <- as.character(as.POSIXct(strptime(dataRaw, "%F %T")))
data <- as.POSIXct(data)

#wyciągam treść
tresc <- sapply(urlArt, function(x){
   nodesTresc <- html_nodes(html(x), ".article-body")
   art <- html_text(nodesTresc)
   return(art)
})

#dokonuje wstępnego oczyszczenia 
#-usuwanie białych spacji
#-usuwanie znaków przystankowych
#-transformacja na małe litery
corpus <- Corpus(VectorSource(tresc))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))


#tworze ramke danych 
dfTematGlowna <- data.frame("tytul"=tytul, "data"=data, 
                            "tresc"=unlist(sapply(corpus, `[`, "content")),
                            "url"=urlArt,
                            stringsAsFactors=FALSE)
rownames(dfTematGlowna) <- NULL

#nazwa z datą pobierania danych
nazwa <- paste(as.character(Sys.Date()), "_NaTematGlowna.txt", sep="")

#zapisuje do pliku txt
write.csv(dfTematGlowna, nazwa, row.names=FALSE)

#dziala :)
#read.csv("2015-03-15_NaTematGlowna.txt")