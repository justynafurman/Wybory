#skrypt do ściągania artykułow z natemat - zakładka o wyborach
#propozycja automatyzacji - uruchamianie skryptu co 5 dni

library(rvest)
library(stringi)
library(tm)

urlTematWybory <- "http://natemat.pl/c/73,wybory"
htmlTematWybory <- html(urlTematWybory)
nodes <- html_nodes(htmlTematWybory, ".without-body-header .hp-content-image+ h2 a")

#wyciagam linki do artykułów
urlArt <- html_attr(nodes, "href")
#indykator czy artykuł jest z natemat czy nie
indNaTemat <- stri_detect_regex(urlArt, ".+natemat.+")
#biorę tylko te, które są z natemat (bo chcę żeby artykuły miały tę samą strukturę)
urlArt <- urlArt[indNaTemat]

#wyciągam tytul
tytul <- html_text(nodes)[indNaTemat]

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
dfTematWybory <- data.frame("tytul"=tytul, "data"=data, 
                            "tresc"=unlist(sapply(corpus, `[`, "content")),
                            "url"=urlArt,
                           stringsAsFactors=FALSE)
rownames(dfTematWybory) <- NULL

#nazwa z datą pobierania danych
nazwa <- paste(as.character(Sys.Date()), "_NaTematWybory.txt", sep="")

#zapisuje do pliku txt
write.csv(dfTematWybory, nazwa, row.names=FALSE)

#dziala :)
#read.csv("2015-03-15_NaTematWybory.txt")