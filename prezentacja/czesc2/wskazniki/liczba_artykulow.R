## Funkcja ile_artykulow rysuje wykres ilosci artykulow o danym kandydacie kazdego dnia.
## Opcjonalne jest ustawienie portali, ktorych dane nas interesują.
## Zliczane są artykuly UNIKATOWE, różne teksty o kandydacie: nie interesuje nas tutaj jak dlugo dany
## artykul wisiał na stronie.
## Jak różne traktujemy dwa artykuly o tej samej treści, jednak pochodzących z różnych miejsc na stronie,
## np. koknretny artykuł o M. Ogórek, który można było przeczytać z linków w newsach i w obrazkach.

all <- c("interia","NaTematGlowna","NaTematWybory","OnetGlowna","OnetWybory","tvn24","wp","wPolityceGlowna","wsieci")

ile_artykulow <- function( osoba, strona=all, kolor="brown", ylim=c(0,100) ){
      require(dplyr)
      require(stringi)
      dane <- read.csv("dane/podsumowanie.txt")
      
      #wybierzemy unikalne artykuly (kluczem unikalnosci bedzie liczba znakow w artykule: dlugosc.art.)
      df <- dane %>% 
                  filter(portal%in%strona, kandydat==osoba) %>%
                  select(portal,kandydat,data,dlugosc.art) %>%
                  unique
      df$day <- as.Date(df$data)
      df$week <- strftime(df$data,"%W")
      df$month <- strftime(df$data,"%m")
      # UWAGA! nie ma wszystkich dat: jesli danego dnia byla sesja w ktorej nie znaleziono
      # artykulu, powinnismy miec 0 artykulow, jesli nie bylo sesji danego dnia mamy NA.
      # Pobieranie przebiegalo w trzech roznych trybach, na dwoch roznych systemach operacyjnych.
      win1 <- c("NaTematGlowna","OnetGlowna","wPolityceGlowna")
      win2 <- c("OnetWybory","NaTematWybory")
      lin <- c("wp","interia","wsieci","tvn24")
      # dlatego nalezy rozroznic wprowadzone przez uzytkownika strony.
      
      ########### FUNKCJE POMOCNICZE ###########
      dni_sesji_trybu <- function(tryb){
            dane %>%
                  filter(portal%in%tryb) %>%
                  select(data) %>%
                  t  %>% 
                  as.Date  %>% 
                  unique
      }
      agregacja_dniami <- function(tryb){
            df %>%
                  filter(portal%in%strona[strona%in%tryb]) %>%
                  group_by( czas = day ) %>%
                  summarise( liczba_artykulow = length(data) )
            # jest length(data) ale może byc (length( dowolna_kolumna )).
            # chodzi o liczbe rekordow w danym przedziale czasu.
      }
      # uzupelnianie zerami
      uzupelnij_zerami <- function(tryb){
            df_trybu <- data.frame(czas2=dni_sesji_trybu(tryb))
            agregacja_dniami(tryb) %>% right_join(df_trybu, by=c("czas" = "czas2")) -> newdf
            newdf[is.na(newdf)] <- 0
            return( arrange(newdf,czas) )
      }
      ###########       ###########       ###########       ########### 

      if( any(strona%in%win1) ){
            a <- uzupelnij_zerami(win1)
            if( any(strona%in%lin) ){
                  b <- uzupelnij_zerami(lin)
                  # poniewaz podany parametr strona zawiera portale z dwoch roznych trybow,
                  # nalezy zsumowac liczbe artykulow zagregowana w dwoch roznych trybach.
                  # zrobimy to na "zjoinowanej" po czasie tabeli.
                  # zalozenie: sesje windowsa zawieraja sesje linuksa. i to jest prawda poki co.
                  c <- full_join(a,b, by="czas") %>% transmute(czas,liczba_artykulow=liczba_artykulow.x+liczba_artykulow.y)
                  d <- as.data.frame(c)
            } else {
                  d <- as.data.frame(a)
            }
      }else{
            d <- as.data.frame(uzupelnij_zerami(lin))
      }
            

      # pelna os czasu
      date_range <- range(as.Date(dane$data))
      day_axis <- seq( date_range[1], date_range[2], by = "day" )
      # hour_axis <- seq( date_range[1], date_range[2], by = "hour" )

      # plotowanie
      # dla czytelnosci zamiast ustawiac miesiace w labels podzielimy obszar wykresu na miesiace:
      kwiecien <- as.Date("2015-04-01")
      maj <- as.Date("2015-05-01")
      plot(liczba_artykulow ~ czas, d,
           type="l", lwd=3, col=kolor, xaxt="n", ylim=ylim, xlab="dzien miesiaca"
           , main=if(all(strona==all)) "all" else strona
           )
      rect(0,-1,kwiecien,200,col=rgb(1,0,0,.1),border = NULL)
      rect(kwiecien,-1,maj,200,col=rgb(0,1,0,.1), border=NULL)
      axis.Date(1, at = day_axis[day_axis%in%d$czas], las=2, format="%d", cex.axis=.8)    
      axis.Date(1, at = day_axis[!(day_axis%in%d$czas)], las=2, format="%d", col.axis="red", cex.axis=.8)  # na czerwono braki danych

}
# przyklad bez NAs
ile_artykulow(osoba="duda",strona="wPolityceGlowna")
ile_artykulow(osoba="duda",strona="wp", ylim=c(0,5))

ile_artykulow(osoba="komorowski",strona="wPolityceGlowna")
ile_artykulow(osoba="komorowski",strona="wp",ylim=c(0,5))
# cos jest nie tak z  tym ze na moich stronach dziennie jest gora piec artykulow, a na twoich i z 60... 
# pomysle nad tym jutro :P

# obsluga NAs
# jesli dla danej daty na przynajmniej jednym portalu nie mamy pobierania, wykres zwroci wartosc NA.
ile_artykulow(osoba="duda",strona=c("wPolityceGlowna","wp"))

#wszystkie
ile_artykulow(osoba="komorowski","wsieci", ylim=c(0,30))
par(new=TRUE)
ile_artykulow(osoba="duda", "wsieci", kolor="green", ylim=c(0,30))
