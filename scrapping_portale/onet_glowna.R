#skrypt do ściągania artykułow z onet - strona główna
#propozycja automatyzacji - uruchamianie skryptu kilka razy dziennie


library(rvest)
library(stringi)
library(tm)

urlOnetGlowna <- "http://www.onet.pl/"
htmlOnetGlowna <- html(urlOnetGlowna)

nodesBest <- html_nodes(htmlOnetGlowna, ".bestOfOnetTop")
tytulBest <- stri_trim_both(html_text(nodesBest))
urlBest <- html_attr(nodesBest, "href")

nodesTop <- html_nodes(htmlOnetGlowna, ".sliderItem")
tytulTop <- stri_trim_both(html_text(nodesTop))
urlTop <- html_attr(nodesTop, "href")

nodes <- html_nodes(htmlOnetGlowna, "#boxNews a")
#poniewaz to co wyzej daje mi na poczatku i na koncu kilka dodatkowych linkow 
#zabezpieczam sie i biore tylko te, ktore chce, a te maja atrybut jak nizej
ind <- html_attr(nodes, "data-staticr")
ind <- !is.na(ind)
nodes <- nodes[ind]

#wyciągam url
urlArt <- html_attr(nodes, "href")

#wyciągam wszystkie tytuly
tytul <- stri_trim_both(html_text(nodes))

#wszystkie tytuly z glównej potencjalnie mogące być o wyborach
urlAll <- c(urlBest, urlTop, urlArt)
tytulAll <- c(tytulBest, tytulTop, tytul)
waznosc <- c("duze", rep("srednie", length(tytulTop)), rep("male", length(tytul)))


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

n <- length(tytulAll)
czyWybory <- logical()
for (i in 1:n){
   tyt <- tytulAll[i]
   czyNazwisko <- sapply(kandydat, function(x){
      stri_detect_regex(tyt, x)
   })
   czyWybory[i] <- any(czyNazwisko)
}

#uaktualniam listę 
urlAll <- urlAll[czyWybory]
tytulAll <- tytulAll[czyWybory]
waznosc <- waznosc[czyWybory]

#wyciagam treść
tresc <- sapply(urlAll, function(x){
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
dfOnetGlowna <- data.frame("tytul"=tytulAll, 
                           "tresc"=unlist(sapply(corpus, `[`, "content")),
                           "url"=urlAll, "waznosc"=waznosc,
                           stringsAsFactors=FALSE)

#nazwa z datą pobierania danych
data <- strftime(Sys.time(),"%Y-%m-%d@%H-%M")
nazwa <- paste(as.character(data), "_OnetGlowna.txt", sep="")

#zapisuje do pliku csv
if(nrow(dfOnetGlowna!=0)){
   write.csv(dfOnetGlowna, nazwa, row.names=FALSE)
}
