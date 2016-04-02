# Plik zawiera dwie funkcje: sumaryczna_waga(), sumaryczna_waga_szczegolwa().
# IDEA: funkcja zwroci tabelke wszystkich kandydatow z sumarycznymi wagami unikatowych artykulow jakie
# sie pojawily.
# Pierwsza funkcja zrobi to bez rozrozniania portali, druga z rozroznieniem portali.
# To z pewnoscia na razie zarodek - mam nadzieje - czegos wielkiego! :)

all <- c("interia","NaTematGlowna","NaTematWybory","OnetGlowna","OnetWybory","tvn24","wp","wPolityceGlowna","wsieci")

sumaryczna_waga <- function(strona=all){
      require(dplyr)
      dane <- read.csv("dane/podsumowanie.txt")
      
      # wybranie unikatowych artykulow. Czasem zdarza sie ten sam artykul w sekcji newsy i obrazek
      # wowczas wydaje sie uzasadnionym traktowac te rekordy jako rozne. Usuwamy zatem te same artykuly,
      # ktore zostaly wielokrotnie pobrane. Jako wyznacznik identycznosci artykulow traktuje
      # liczbe znakow w nim zawarta.
      dane %>%
            filter(portal%in%strona) %>%
            select(portal,kandydat,dlugosc.art,waga) %>%
            unique -> df
      return( sort(tapply(df$waga,df$kandydat,sum), decreasing = TRUE) )
}

sumaryczna_waga("wp")

sumaryczna_waga_szczegolowa <- function(strona=all){
      require(dplyr)
      dane <- read.csv("dane/podsumowanie.txt")
      
      # wybranie unikatowych artykulow. Czasem zdarza sie ten sam artykul w sekcji newsy i obrazek
      # wowczas wydaje sie uzasadnionym traktowac te rekordy jako rozne. Usuwamy zatem te same artykuly,
      # ktore zostaly wielokrotnie pobrane. Jako wyznacznik identycznosci artykuolow traktuje
      # liczbe znakow w nim zawarta.
      dane %>%
            filter(portal%in%strona) %>%
            select(portal,kandydat,dlugosc.art,waga) %>%
            unique -> df
      wynik <- sapply(split(df,df$kandydat), function(x) tapply(x$waga,x$portal,sum))
      wynik <- replace(wynik,is.na(wynik),0)  # usuwanie NAs. W tym przypadku oznaczaja one zerowa wage.
      return( wynik[strona,] )
}

sumaryczna_waga_szczegolowa()
sumaryczna_waga_szczegolowa(strona=c("wp","interia"))
