library(stringi)
library(wordcloud)
library(dplyr)

setwd("D:/GoogleDrive/Studia/SemestrVIII/R-i-Big-Data/Wybory/dane")
artykuly <- read.csv("artykuly.txt")

#na razie rozwazam tylko filtrowanie po portalu i kandydacie
#w kolejnej fazie weźmiemy również pod uwagę jak to się zmienia czasowo
#domyślnie do chmury bierzemy 150 njczęstszych słów
#i - kolumna w której znajduje się treść (funkcja aplikuje sie również do twittow czy facebooka)
zrobChmure <- function(art, port="", kand="", czas=c(), 
                       ile.slow=150, i=5){
   if(port != "") art <- art %>% filter(portal == port)
   if(kand != "") art <- art %>% filter(kandydat == kand)
   
   tresc <- art[, i]
   words <- unlist(stri_extract_all_words(tresc))
   tab <- table(words)
   tab.sort <- sort(tab, dec=TRUE)
   do.chmury <- tab.sort[1:ile.slow]

   pal2 <- brewer.pal(8,"Set2")
   wordcloud(names(do.chmury), do.chmury, scale=c(5.5, 0.4), max.words=Inf, 
             random.order=F, rot.per=.3, colors=pal2)
   tab.podst <- tab.sort[1:10]
   return(tab.podst)
}

zrobChmure(artykuly)
zrobChmure(artykuly, port="wsieci", kand="duda", ile.slow=100)
zrobChmure(artykuly, port="wsieci", kand="komorowski", ile.slow=100)
zrobChmure(artykuly, port="wsieci", kand="ogorek", ile.slow=100)
zrobChmure(artykuly, kand="korwin")
zrobChmure(artykuly, kand="ogorek")
