#skrypt do porównywania ramek danych z różnych dat ściągania
#w celu uniknięcia zduplikowania artykułów

#idea: argumentem funkcji są dwie ramki danych z tego samego portalu ale z różnych dat (dwóch kolejnych)
#podaję je chronologicznie i patrze czy w drugiej sa wiersze, które były już w pierwszej
#jeżeli tak to je usuwam

#na pewno chcę tego użyć do zakładek wybory, na stronach głównych raczej nie
#bo w sumie może warto trzymać powtórki, wtedy będzie można wyciągnąć jak długo artykuł był na głównej czy coś

#wektorSpr - numery kolumn, ktore będziemy porównywać (czasami jedna kolumna może nie wystarczyć)

setwd("D:/GoogleDrive/Studia/SemestrVIII/R-i-Big-Data/Wybory/dane/") #wybrac podfolder
a <- list.files(pattern="[^\\.]*.txt")
n <- length(a)
dfNowa <- read.csv(a[n]) #ostatnia
dfStara <- read.csv(a[n-1]) #przedostatnia
names(dfNowa)
wektorSpr <- 4 #do zmiany

porownajRamki(dfStara, dfNowa, wektorSpr)

#funkcja
porownajRamki <- function(dfStara, dfNowa, wektorSpr){
   nNowa <- dim(dfNowa)[1]
   ind <- logical(nNowa)
   for (i in 1:nNowa){
      ind[i] <- !is.element(dfNowa[i, wektorSpr], dfStara[,wektorSpr])
   }
   dfNowa <- dfNowa[ind,]
   write.csv(dfNowa, a[n], row.names=FALSE)
}
