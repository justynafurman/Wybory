# hej Justyna! Nie musisz wdrazac sie w kod (jesli nie chcesz :P) zalezy mi tylko na tym
# zebys odpalila i zobaczyla jak to wyglada i czy moze byc.

# funkcja ma pokazywac istnienie kandydata w czasie i na portalu.
# tzn czy byl obecny i w jakich godzinach (czy szczytu? :P)
# i jak czesto. Dlatego nie jest tutaj wazne, ze artykuly sie duplikuja,
# tylko czy mozna bylo o nim przeczytac o danej godzinie.

# problemem tutaj (i wszedzie tak naprawde) jest to, ze brak punktu nie oznacza
# ze o kandydacie nie pisano. Wszak moze byc, ze nie bylo pobierania o danej porze.


# istnienie kandydydata w danym czasie na stronie/stronach
istnienie_kandydata <- function(osoba,strona,kolor="black",po_czym="waga"){
      require(dplyr)
      dane <- read.csv("dane/podsumowanie.txt")
      dane %>%
            filter(portal%in%strona,kandydat==osoba) %>%
            select(kandydat,portal,data,contains(po_czym)) -> df2
      if( nrow(df2)>0 ){
            as.POSIXct(df2$data) -> df2$data
            
            # dla czytelnosci zamiast ustawiac miesiace w labels podzielimy obszar wykresu na miesiace:
            kwiecien <- as.POSIXct("2015-04-01 01:00:00")
            maj <- as.POSIXct("2015-05-01 01:00:00")
            # pelna os czasu
            date_range <- strptime( range(df2$data), format="%F" )
            hour_axis <- seq( date_range[1], date_range[2], by = "hour" )
            day_axis <- seq( date_range[1], date_range[2], by = "day" )
            
            plot( df2$data,t(df2[po_czym]), xaxt="n", type="p", pch=1, cex=.5, col=kolor,xlab="", ylab=po_czym)
            rect(0,-1,kwiecien,100,col=rgb(1,0,0,.2),border = NULL)
            rect(kwiecien,-1,maj,100,col=rgb(0,1,0,.2), border=NULL)
            axis.POSIXct(side=1, at=hour_axis[rep(c(T,F,F,F,F,F),2)],
                         format="%H",las=1, cex.axis=0.75, )  
            axis.POSIXct(side=3, at=day_axis,
                         format="%d",las=1, cex.axis=0.75, "days")  
            mtext(side=1, "hours", line=2.5)
            mtext(side=3, "days", line=2.5)
      }else{
            cat("brak informacji o kandydacie")
      }
}

## TESTY
head(dane)
# proste dzialanie
istnienie_kandydata(osoba="komorowski", strona="tvn24", kolor="red", "waga")
# dwa porale
istnienie_kandydata(osoba="komorowski", strona=c("wp","interia"))
# TRZY portale :D
istnienie_kandydata(osoba="komorowski", strona=c("wp","interia","tvn24"))
# inny wskaźnik
istnienie_kandydata(osoba="komorowski", strona="tvn24", kolor="red", "dlugosc.art")

istnienie_kandydata(osoba="korwin", strona="tvn24", kolor="red")
