library(rvest)
library(stringi)
library(tm)

urlStrona <- "http://wpolityce.pl/"
html <- html(urlStrona)

#pobiera tytuły, date, url do odpowiedniego typu artykułu

#css - część css (ta która rozróżnia wielkość artykułu)
#typ - napis, który chcemy dodać przy danem typie artykułu, np. "top", "duże"...
pobierz <- function(css, typ){
   tytul <- html_nodes(html, paste(css, " a h3", sep=""))
   tytul <- html_text(tytul)
   data <- html_nodes(html, paste(css, " .js-relative-date", sep=""))
   data <- html_text(data)
   data <- as.POSIXct(data)
   url <- html_nodes(html, paste(css, " a", sep=""))
   url <- html_attr(url, "href")
   url <- paste(urlStrona, stri_sub(url, 2), sep="")
   return(data.frame("tytul"=tytul, "data"=data, "url"=url, "waznosc"=typ,
                     stringsAsFactors=FALSE))
}

cssTop <- ".single-headline-3col"
cssTop2 <- ".double-headline-3col"
cssDuze <- ".article-with-wide-thumb-2col"
cssSrednie <- ".article-with-narrow-thumb-2col"
cssMale <- ".article-with-thumb-1col"
cssMini <- ".box-x-3 .article-1col"

dfAll <- pobierz(cssTop, "top")
dfAll <- rbind(dfAll, pobierz(cssTop2, "top2"))
dfAll <- rbind(dfAll, pobierz(cssDuze, "duze"))
dfAll <- rbind(dfAll, pobierz(cssSrednie, "srednie"))
dfAll <- rbind(dfAll, pobierz(cssMale, "male"))
dfAll <- rbind(dfAll, pobierz(cssMini, "mini"))

#mam ramkę danych ze wszystkimi artykułami wraz ze znacznikiem wielkości artyukuł
#teraz wyeliminiuję te artykuły, które nie dotyczą żadnego z kandydatów

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

n <- length(dfAll$tytul)
czyWybory <- logical()
for (i in 1:n){
   tyt <- dfAll$tytul[i]
   czyNazwisko <- sapply(kandydat, function(x){
      stri_detect_regex(tyt, x)
   })
   czyWybory[i] <- any(czyNazwisko)
}

#uaktualniam ramkę
dfAll <- dfAll[czyWybory,]

#teraz muszę jeszcze powyciągać treści wybranych już artykułów
#wyciagam treść
tresc <- sapply(dfAll$url, function(x){
   nodes <- html_nodes(html(x), ".bbtext")
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

#dodaje kolumne z trescia
dfAll$tresc <- unlist(sapply(corpus, `[`, "content"))

#nazwa z datą pobierania danych
data <- strftime(Sys.time(),"%Y-%m-%d@%H-%M")
nazwa <- paste(as.character(data), "_wPolityceGlowna.txt", sep="")

#zapisuje do pliku csv
write.csv(dfAll, nazwa, row.names=FALSE)