## PRZEGLAD NAGLOWKOW ##
# Funkcja zlicza ile razy w naglowku wystapilo nazwisko danego kandydata.
# Nie uwgledniamy unikatowych artykulow ale to, czy gdy sie weszlo o dowolnej porze do internetu
# nazwisko kandydata uderzalo nas po oczach ze wszystkich wokolo naglowkow.

all <- c("interia","NaTematGlowna","NaTematWybory","OnetGlowna","OnetWybory","tvn24","wp","wPolityceGlowna","wsieci")

przeglad_naglowkow <- function(strona=all){
      require(dplyr)
      dane <- read.csv("dane/podsumowanie.txt")
      dane %>%
            filter(portal%in%strona) %>%
            select(portal,kandydat,czy.w.tytule) %>%
            as.data.frame  %>% 
            split(.$kandydat) %>%
            sapply(function(ramka_z_danej_strony)
                  table(ramka_z_danej_strony$portal[ramka_z_danej_strony$czy.w.tytule])
                  ) -> df
      return( df[strona,] )
}

# ze wszystkich portali:
przeglad_naglowkow()
# z wybranych portali:
przeglad_naglowkow(strona=c("tvn24","wsieci"))
