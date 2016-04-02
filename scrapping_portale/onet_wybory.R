#skrypt do ściągania artykułów z zakładki wybory prezydenckie z onetu
#propozycja automatyzacji - uruchamianie skryptu co 5 dni

library(rvest)
library(stringi)
library(tm)

urlOnetWybory <- "http://wiadomosci.onet.pl/wybory-prezydenckie/xcnpc"
htmlOnetWybory <- html(urlOnetWybory)
nodes <- html_nodes(htmlOnetWybory, ".datePublished , .itemTitle")
text <- html_text(nodes)

#wyciągam datę
dataRaw <- text[seq(1, length(text), 2)]
data <- character(length(dataRaw))

indMinuty <- stri_detect_regex(dataRaw, "[0-9]{1,2} min.+")
indDzisiaj <- stri_detect_regex(dataRaw, "dzisiaj [0-9]{2}:[0-9]{2}")
indWczoraj <- stri_detect_regex(dataRaw, "wczoraj [0-9]{2}:[0-9]{2}")
indReszta <- !(indWczoraj | indDzisiaj | indMinuty)

minuty <- as.numeric(stri_sub(dataRaw[indMinuty], 1, 2))
data[indMinuty] <- as.character(Sys.time() - 60*minuty)
data[indDzisiaj] <- stri_replace_all_fixed(dataRaw[indDzisiaj], "dzisiaj", as.character(Sys.Date()))
data[indWczoraj] <- stri_replace_all_fixed(dataRaw[indWczoraj], "wczoraj", as.character(Sys.Date() - 1))
data[indReszta] <- as.character(as.POSIXct(strptime(dataRaw[indReszta], "%d %b, %H:%M")))
data <- as.POSIXct(data)


#wyciągam tytuł
tytul <- text[seq(2, length(text), 2)]

#wyciągam treść artykułów
nodes <- html_nodes(htmlOnetWybory, ".listItemSolr")
urlArt <- sapply(html_children(nodes), function(x){
   html_attr( x$a, "href")
})
tresc <- sapply(urlArt, function(x){
   nodes <- html_nodes(html(x), ".hyphenate")
   art <- paste(html_text(nodes), collapse=" ")
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
dfOnetWybory <- data.frame("tytul"=tytul, "data"=data, 
                           "tresc"=unlist(sapply(corpus, `[`, "content")),
                           "url"=urlArt,
                           stringsAsFactors=FALSE)

#nazwa z datą pobierania danych
nazwa <- paste(as.character(Sys.Date()), "_OnetWybory.txt", sep="")

#zapisuje do pliku csv
write.csv(dfOnetWybory, nazwa, row.names=FALSE)

# działa :) read.csv("2015-03-15_OnetWybory.txt")
