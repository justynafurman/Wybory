library(shiny)
library(dplyr)
library(stringi)
library(wordcloud)
library(tidyr)
library(ggmap)

shinyServer(function(input, output, session) {
   
   nazwiska <- c("komorowski","kowalski","duda","palikot","jarubas","ogorek","korwin","wilk","braun","kukiz")
   artykuly <- read.csv("artykuly.txt")
   artykuly$data <- strftime(strptime(artykuly$data, "%F %T"), "%F %T")
   podsumowanie <- read.csv("podsumowanie.txt")
   podsumowanie$data <- strftime(strptime(podsumowanie$data, "%F %T"), "%F %T")
   stopwords <- "itp, itd, mógł, kogo, chcą, xd, niz, x, np, tę, cos, zl, ps, mial, miał, byc, być, maja, beda, raz, nikt, chce, go, sa, da, tez, ha, juz, bez, samo, p, sam, a, aby, ach, acz, aczkolwiek, aj, albo, ale, ależ, ani, aż, bardziej, bardzo, bo, bowiem, by, byli, bynajmniej, być, był, była, było, były, będzie, będą, cali, cała, cały, ci, cię, ciebie, co, cokolwiek, coś, czasami, czasem, czemu, czy, czyli, daleko, dla, dlaczego, dlatego, do, dobrze, dokąd, dość, dużo, dwa, dwaj, dwie, dwoje, dziś, dzisiaj, gdy, gdyby, gdyż, gdzie, gdziekolwiek, gdzieś, i, ich, ile, im, inna, inne, inny, innych, iż, ja, ją, jak, jakaś, jakby, jaki, jakichś, jakie, jakiś, jakiż, jakkolwiek, jako, jakoś, je, jeden, jedna, jedno, jednak, jednakże, jego, jej, jemu, jest, jestem, jeszcze, jeśli, jeżeli, już, ją, każdy, kiedy, kilka, kimś, kto, ktokolwiek, ktoś, która, które, którego, której, który, których, którym, którzy, ku, lat, lecz, lub, ma, mają, mało, mam, mi, mimo, między, mną, mnie, mogą, moi, moim, moja, moje, może, możliwe, można, mój, mu, musi, my, na, nad, nam, nami, nas, nasi, nasz, nasza, nasze, naszego, naszych, natomiast, natychmiast, nawet, nią, nic, nich, nie, niech, niego, niej, niemu, nigdy, nim, nimi, niż, no, o, obok, od, około, on, ona, one, oni, ono, oraz, oto, owszem, pan, pana, pani, po, pod, podczas, pomimo, ponad, ponieważ, powinien, powinna, powinni, powinno, poza, prawie, przecież, przed, przede, przedtem, przez, przy, roku, również, sama, są, się, skąd, sobie, sobą,  sposób, swoje, ta, tak, taka, taki, takie, także, tam, te, tego, tej, temu, ten, teraz, też, to, tobą, tobie, toteż, trzeba, tu, tutaj, twoi, twoim, twoja, twoje, twym, twój, ty, tych, tylko, tym, u, w, wam, wami, was, wasz, wasza, wasze, we, według, wiele, wielu, więc, więcej, wszyscy, wszystkich, wszystkie, wszystkim, wszystko, wtedy, wy, właśnie, z, za, zapewne, zawsze, ze, zł, znowu, znów, został, żaden, żadna, żadne, żadnych, że, żeby, pan, pana, panie, sie, panu, wam, "
   
   ##### przefiltrowane dane #####
   artykuly.new <- reactive({
      artykuly %>%
         filter(portal %in% input$Portal) %>%
         filter(kandydat %in% input$Kandydat) %>%
         filter(data >= input$data[1] & data <= input$data[2])
   })
   podsumowanie.new <- reactive({
      podsumowanie %>%
         filter(portal %in% input$Portal) %>%
         filter(kandydat %in% input$Kandydat) %>%
         filter(data >= input$data[1] & data <= input$data[2])
   })
   podsumowanie.new.unique <- reactive({
      tmp <- podsumowanie.new() %>%
         select(portal, kandydat, czy.w.tytule, dlugosc.art, ile.razy, ile.razy.wzg, waga)
      podsumowanie.new()[!duplicated(tmp),]
   })
   fbLikes <- reactive({
         profile_glowne <- c("Bronisław Komorowski", "Marian Kowalski - Mój Prezydent", "Andrzej Duda",
                             "Like dla Janusza Palikota", "Adam Jarubas", "Magdalena Ogórek na Prezydenta RP", "Janusz Korwin-Mikke",
                             "Jacek Wilk", "Grzegorz Braun", "Prezydent Kukiz", "Paweł Tanajno" )
#          nazwiska <- c("komorowski","kowalski","duda","palikot","jarubas", "ogorek","korwin","wilk","braun","kukiz")
         names(profile_glowne) <- nazwiska
         wczytaj <- function( czyje ){
               read.table(paste0("Facebook/Likes/",czyje,".csv"), header=TRUE)[,c(2,3,5)]
         }
         lista_danych <- lapply(input$Kandydat,wczytaj)
         dane <- do.call("rbind",lista_danych)

         dane %>%
               filter(name%in%profile_glowne[input$Kandydat]) %>%
               distinct(date,name) %>%
               group_by(date=as.Date(date), name=as.factor(name)) %>% 
               droplevels()
   })
   fbCommentsCloud <- reactive({
         wczytaj <- function( czyje ){
               as.character(read.table(paste0("Facebook/",czyje,"_comments.csv"), header=TRUE)[,4])
         }
         lista_danych <- lapply(input$Kandydat,wczytaj)
         dane <- do.call("c",lista_danych)
   })
   twitter_lon_lat <- reactive({
            wczytaj <- function( czyje ){
                  df <- read.csv(paste0("twitter_rozdzielone2/",czyje,".csv"))[,c("lon","lat")]
                  df <- cbind(df,kto=czyje)
            }
            lista_danych <- lapply(input$Kandydat,wczytaj)
            dane <- do.call("rbind",lista_danych)
            dane <- dane[!is.na(dane$lon),]
      })
   ##### koniec przefiltrowanych danych #####
   
   
   
   ##### wskaźniki i funkcje pomocnicze dla wskaźników #####
   uporzadkujDoChmury <- function( tresc, ile.slow ){
      # data cleaning
      tresc <- stri_replace_all_regex(tresc, "http(s)*://[^ ]+","")
      tresc <- stri_replace_all_regex(tresc, "www.[^ ]+","")
      tresc <- stri_replace_all_regex(tresc, "[0-9]*","")
      tresc <- stri_replace_all_regex(tresc, " if ","")
      tresc <- stri_replace_all_regex(tresc, " if ","")
      tresc <- stri_replace_all_regex(tresc, "undefined","")
      tresc <- stri_replace_all_regex(tresc, "entryhit","")
      tresc <- stri_replace_all_regex(tresc, "var","")
      tresc <- stri_replace_all_regex(tresc, "video","")
      tresc <- stri_replace_all_regex(tresc, "wideo","")
      tresc <- stri_replace_all_regex(tresc, "widarticleajaxhtmllocalversionabarticlenum","")
      tresc <- stri_replace_all_regex(tresc, "typeof","")
      tresc <- stri_replace_all_regex(tresc, "documentgetelementbyidreksrdstyledisplayblock","")
      tresc <- stri_trans_tolower(tresc)

      words <- unlist(stri_extract_all_words(tresc, omit_no_match = TRUE))
      # usuwanie stopwords
      words <- sapply(words, function(x) if(!any(stri_detect_regex(stopwords,x))) x else NA, simplify = TRUE)
      wyrazy_bez_stopwords <- words[!is.na(words)]
      
      tab <- table(wyrazy_bez_stopwords)
      tab.sort <- sort(tab, dec=TRUE)
      do.chmury <- tab.sort[1:ile.slow]
      tab.podst <- tab.sort[1:10]
      tab.podst <- t(as.data.frame(slowa = names(tab.podst), tab.podst))
      row.names(tab.podst) <- NULL
      return(list(do.chmury, tab.podst))
   }
   
   zrobChmure <- function(do.chmury){
      pal2 <- brewer.pal(8,"Set2")
      wordcloud(names(do.chmury), do.chmury, scale=c(5.5, 0.4), 
                random.order=F, rot.per=.3, colors=pal2)
   }
   
   sumaryczna_waga <- function(pods){
         if(input$waga_co == "sumaryczna waga"){
               suma <- tapply(pods$waga, pods$kandydat, sum)
               suma.sort <- sort(suma, decreasing = TRUE)
               barplot(suma.sort, main = "Sumaryczna waga dla unikalnych artykułów \n dla wybranych kandydatów i portali",
                       ylim = c(0, 3500))
         } else {
               srednia <- tapply(pods$waga, pods$kandydat, mean)
               srednia.sort <- sort(srednia, decreasing = TRUE)
               barplot(srednia.sort, main = "Średnia waga dla unikalnych artykułów \n dla wybranych kandydatów i portali",
                       ylim = c(0, 3))
         }  
   }
   
   sumaryczna_waga_szczegolowa <- function(pods){
            wynik <- pods %>%
                  group_by(portal, kandydat) 
            
            if(input$waga_co == "sumaryczna waga") {
                  wynik <- wynik %>%
                        summarise(waga = sum(waga))
            } else {
                  wynik <- wynik %>%
                        summarise(waga = mean(waga))
            }
            
            wynik <- spread(wynik, kandydat, waga)
            wynik <- replace(wynik,is.na(wynik), 0)
            return(wynik)
      }
   
   heatmap_sumaryczna_waga_szczegolowa <- function(){

         podsumowanie %>%
               select(portal,kandydat,dlugosc.art,waga) %>%
               unique -> df
         if(input$waga_co == "sumaryczna waga") {
               wynik <- sapply(split(df,df$kandydat), function(x) tapply(x$waga,x$portal,sum))
         } else {
               wynik <- sapply(split(df,df$kandydat), function(x) tapply(x$waga,x$portal,mean))
         }
         wynik <- replace(wynik,is.na(wynik),0) 
         return( wynik )
         
#          pods %>%
#                select(portal,kandydat,dlugosc.art,waga) %>%
#                unique -> df
#          wynik <- sapply(split(df,df$kandydat), function(x) tapply(x$waga,x$portal,sum))
#          wynik <- replace(wynik,is.na(wynik),0) 
#          return( wynik )
   }
   
przeglad_naglowkow <- function(pods){
      if(input$naglowki_co == "ile razy w tytule") {
            suma <- tapply(pods$czy.w.tytule, pods$kandydat, sum)
            suma.sort <- sort(suma, decreasing = TRUE)
            barplot(suma.sort, main = "Ilość wystąpień nazwiska kandydata w tytule",
                    ylim = c(0, 2600))
      } else {
            srednia <- tapply(pods$czy.w.tytule, pods$kandydat, mean)
            srednia.sort <- sort(srednia, decreasing = TRUE)
            barplot(srednia.sort, main = "Względna ilość wystąpień nazwiska kandydata w tytule",
                    ylim = c(0, 1))
      }
}

###nagłówki - tabela
przeglad_naglowkow_szczegolowy <- function(pods){
      wynik <- pods %>%
            group_by(portal, kandydat) 
      if(input$naglowki_co == "ile razy w tytule") {
            wynik <- wynik %>%
                  summarise(ile.w.tytule = sum(czy.w.tytule))
      } else {
            wynik <- wynik %>%
                  summarise(ile.w.tytule = mean(czy.w.tytule))
      }
      wynik <- spread(wynik, kandydat, ile.w.tytule)
      wynik <- replace(wynik, is.na(wynik), 0)
      return(wynik)
}

###treści - wykres
przeglad_tresci <- function(pods){
      wynik <- pods %>%
            group_by(kandydat)
      if(input$tresc_co == "ile razy w sumie nazwisko w artykule") {
            wynik <- wynik %>%
                  summarise(ile = sum(ile.razy, na.rm= TRUE)) %>%
                  arrange(desc(ile))
            barplot(wynik$ile, names.arg = wynik$kandydat, main = "Sumaryczna liczba wystąpień nazwiska kandydata w artykułach", ylim = c(1, 9000))
      } else if (input$tresc_co == "ile razy średnio nazwisko w artykule"){
            wynik <- wynik %>%
                  summarise(ile = mean(ile.razy, na.rm = TRUE)) %>%
                  arrange(desc(ile))
            barplot(wynik$ile, names.arg = wynik$kandydat, main = "Sumaryczna liczba wystąpień nazwiska kandydata w artykułach", ylim = c(1, 8))
      } else if (input$tresc_co == "sumaryczna dlugość artykułów") {
            wynik <- wynik %>%
                  summarise(ile = sum(dlugosc.art, na.rm = TRUE)) %>%
                  arrange(desc(ile))
            barplot(wynik$ile, names.arg = wynik$kandydat, main = "Całkowita długość artykułów o kandydacie", ylim = c(1, 5000000))
      } else {
            wynik <- wynik %>%
                  summarise(ile = mean(dlugosc.art, na.rm = TRUE)) %>%
                  arrange(desc(ile))
            barplot(wynik$ile, names.arg = wynik$kandydat, main = "Średnia długość artykułu o kandydacie", ylim = c(1, 4000))
      }
}

### treści - tabela
przeglad_tresci_szczegolowy <- function(pods) {
      wynik <- pods %>%
            group_by(portal, kandydat)
      if(input$tresc_co == "ile razy w sumie nazwisko w artykule") {
            wynik <- wynik %>%
                  summarise(ile = sum(ile.razy))
      } else if (input$tresc_co == "ile razy średnio nazwisko w artykule"){
            wynik <- wynik %>%
                  summarise(ile = mean(ile.razy))
      } else if (input$tresc_co == "sumaryczna dlugość artykułów") {
            wynik <- wynik %>%
                  summarise(ile = sum(dlugosc.art))
      } else {
            wynik <- wynik %>%
                  summarise(ile = mean(dlugosc.art))
      }
      wynik2 <- spread(wynik, kandydat, ile)
      wynik2 <- replace(wynik2, is.na(wynik2), 0)
      return(list(wynik, wynik2))
}

###wykresy liniowe - skumulowane wksaźniki, ale tylko dla sum!
# tzn. skumulowane wagi, ilość wstąpień nazwiska w tytule czy artykule
liniowy <- function(pods, po_czym = 9, title = ""){     
      df2 <- pods %>%
            group_by(data) 
      df2 <- df2 %>%
            summarise(nowe = sum(po_czym, na.rm = TRUE))
      df2$nowe <- cumsum(df2$nowe)   
      df2$data <- as.POSIXct(df2$data)
      
      kwiecien <- as.POSIXct("2015-04-01 01:00:00")
      maj <- as.POSIXct("2015-05-01 01:00:00")
      date_range <- strptime( range(df2$data), format="%F" )
      hour_axis <- seq( date_range[1], date_range[2], by = "hour" )
      day_axis <- seq( date_range[1], date_range[2], by = "day" )
      
      plot(df2$data, t(df2["nowe"]), xaxt="n", type="l", lwd= 3, xlab="", ylab = "", main = title)
      
      rect(0,-1000,kwiecien,20000,col=rgb(1,0,0,.2),border = NULL)
      rect(kwiecien,-1000,maj,20000,col=rgb(0,1,0,.2), border=NULL)  
      axis.POSIXct(side=1, at=day_axis,
                   format="%d",las=1, cex.axis=0.75, "days")  
      mtext(side=1, "days", line=2.5)
}
   
   ##### pomocnicze funkcje facebooka ####
   plotuj_lajki <- function( dane ){
         nazwiska <- c("komorowski","kowalski","duda","palikot","jarubas", "ogorek","korwin","wilk","braun","kukiz")
         profile_glowne <- c("Bronisław Komorowski", "Marian Kowalski - Mój Prezydent", "Andrzej Duda",
                             "Like dla Janusza Palikota", "Adam Jarubas", "Magdalena Ogórek na Prezydenta RP", "Janusz Korwin-Mikke",
                             "Jacek Wilk", "Grzegorz Braun", "Prezydent Kukiz", "Paweł Tanajno" )
         kolory <- c("black","red","lightsalmon4","deeppink","darkgreen","olivedrab","darkblue", "gray30", "brown", "orange", "yellow")
         names(kolory) <- profile_glowne
         names(profile_glowne) <- nazwiska
         
         if( length(nazwiska%in%input$Kandydat)==0 ){
               invisible(NULL)
         }else{
              do_wykresu <- dane
               
               # rysujemy kilka wykresow na jednym.
               # 1. krok: plot dla kandydata z najwieksza liczba lajkow
               # 2. krok: lines-y dla reszty kandydatow.
               
               # krok 1.
               kto_najwiecej <- which.max(do_wykresu$likes)
               ile_najwiecej <- do_wykresu$likes[kto_najwiecej]
               level1 <- as.character(do_wykresu$name[kto_najwiecej])
               
               do_wykresu %>%
                     filter(name==level1) -> kandydat1
               plot(likes~date, data=kandydat1,
                    type="l", lwd=3, col=kolory[level1],
                    ylim=c(0,signif(ile_najwiecej,1)*1.5),
                    xlim=c(as.Date("2015-04-05"),as.Date("2015-06-5")),
                    axes=F, ylab="", xlab="",
                    main="Liczba lajków najpopularniejszych portali kandydatów"
               )
               
               # krok 2.
               level2 <- levels(do_wykresu$name)[levels(do_wykresu$name)!=level1]
               for(reszta in level2){
                     do_wykresu %>%
                     filter(name==reszta) -> kandydat
                     lines(likes~date, data=kandydat, col=kolory[reszta], lwd=3)
               }
               box()
               at1 <- seq(from=as.Date("2015-04-06"), to=as.Date("2015-05-12"), by=7)
               axis(1, at=at1, labels=format(at1, "%b %d"))
              
              if(ile_najwiecej>0 & ile_najwiecej<1000){
                    at2 <- seq(0,1000,100)
                    axis(2,at=at2, labels=paste0(at2/100, "00"), las=2)
              }
              if(ile_najwiecej>=1000 & ile_najwiecej<10000){
                    at2 <- seq(0,10000,1000)
                    axis(2,at=at2, labels=paste0(at2/1000, "tys."), las=2)
              }
              if(ile_najwiecej>=10000 & ile_najwiecej<100000){
                    at2 <- seq(0,100000,10000)
                    axis(2,at=at2, labels=paste0(at2/10000, "0tys."), las=2)
              }
              if(ile_najwiecej>=100000 & ile_najwiecej<700000){
                    at2 <- seq(0,1000000,50000)
                    axis(2,at=at2, labels=paste0(at2/1000, "tys."), las=2)
              }
#                at2 <- seq(0,ile_najwiecej,100000)
#                axis(2, at=at2, labels=paste0(at2/100000,"00tys."), las=2)

               y <- do_wykresu %>%
                     filter(date=="2015-05-12") %>% as.data.frame 
               text(as.Date("2015-05-14"), y$likes, 
                    y$name,
                    adj=c(0,0), col=kolory[as.character(y$name)], cex=1.2)
         }
}

   kto_kogo <- function(){
         ludzie <- lapply(nazwiska, 
                          function( kto ) 
                                #read.table(paste0(sciezka,"Facebook/",kto,"_comments.csv"), h=T)[,2])
                             read.table(paste0("Facebook/",kto,"_comments.csv"), h=T)[,2])
         names(ludzie) <- nazwiska
         
         df <- data.frame()
         for( kandydat in nazwiska )
               df <- rbind(df,
                           sapply( ludzie, function( komentator2 ) sum(komentator2%in%ludzie[[kandydat]])/length(ludzie[[kandydat]] ) ))
         colnames(df) <- nazwiska
         rownames(df) <- nazwiska
         matrix <- as.matrix(df)
         diag(matrix) <- 0
         return(list(wykres=matrix,tabela=df))
   }

     kto_kogo_dane <- kto_kogo()
   ##### koniec wskaźników i funkcji pomocniczych do wskaźników #####
   
   
   ##### odpowiedź na kontrolki #####

output$suma_wag_plot <- renderPlot({ 
      if (input$typ == "słupkowy") {
            sumaryczna_waga(podsumowanie.new.unique())
      } else {
            liniowy(podsumowanie.new.unique(), 9, title = "Skumulowane wagi artykułów")
      } 
})

output$suma_wag_table <- renderTable({
      if(input$typ == "słupkowy") sumaryczna_waga_szczegolowa(podsumowanie.new.unique())
})

output$heatMap_sum <- renderPlot({
      isolate({
            heatmap(
                  heatmap_sumaryczna_waga_szczegolowa(),
                  Rowv=NA, Colv=NA, col=colorRampPalette(c("white", "red"))(1000),
                  margins=c(5,5), main="Jak często i jak ważne artykuły o kandydatach pisały portale")
      })
})

output$naglowki_plot <- renderPlot({
      if (input$typ == "słupkowy") {
            przeglad_naglowkow(podsumowanie.new()) 
      } else {
            liniowy(podsumowanie.new.unique(), 5, title = "Skumulowana ilość wystąpień nazwiska w tytule")
      }
})

output$naglowki_table <- renderTable({
      if(input$typ == "słupkowy") przeglad_naglowkow_szczegolowy(podsumowanie.new()) 
})

output$tresc_plot <- renderPlot({
      if(input$typ == "słupkowy") {
            przeglad_tresci(podsumowanie.new.unique())
      } else {
            liniowy(podsumowanie.new.unique(), 7, title = "Skumulowana ilość wystąpień nazwiska w artykułach")
      } 
})

output$tresc_table <- renderTable({
      if(input$typ == "słupkowy") przeglad_tresci_szczegolowy(podsumowanie.new.unique())[[2]]
})

output$chmura <- renderPlot({
     input$goButton
     isolate({
        dane <- uporzadkujDoChmury(artykuly.new()[,5], 150)
        zrobChmure(dane[[1]])
     })
  })
  
output$czestosc <- renderTable({
     input$goButton
     isolate({
        dane <- uporzadkujDoChmury(artykuly.new()[,5], 150)
        dane[[2]]
     })
  })


  #tak, żeby można było zobaczyć jak wyglądają surowe dane (już przefiltrowane)
  output$tabelaPods <- renderTable({
     podsumowanie.new()
  })
  
  output$tabelaArt <- renderTable({
     artykuly.new()
  })
  
  ### FACEBOOK ###
  output$fb_lajki <- renderPlot({
              plotuj_lajki( fbLikes() )
  })

  output$fb_kto_kogo1 <- renderPlot({
      isolate({
      heatmap(kto_kogo_dane$wykres,
            Rowv=NA, Colv=NA, col=colorRampPalette(c("white", "blue"))(1000),
            margins=c(5,5), main="Czy Ci sami ludzie komentują na profilach różnych kandydatów?")
      })
})

  output$fb_kto_kogo2 <- renderTable({
      kto_kogo_dane$tabela
})
      
  output$fb_chmura <- renderPlot({
        input$goButton
        isolate({
              dane <- uporzadkujDoChmury(as.character(fbCommentsCloud()), 150)
              zrobChmure(dane[[1]])
        })
  })

  output$fb_czestosc <- renderTable({
      input$goButton
      isolate({
            dane <- uporzadkujDoChmury(as.character(fbCommentsCloud()), 150)
            dane[[2]]
      })
  })
      
      ### Twitter ###
output$twitter_mapa <- renderPlot({
      dane <- twitter_lon_lat()
      
      dane %>%
            filter(lon>10 & lon <30) %>% 
            filter(lat>47 & lat<55) -> dane
      qmplot(lon,lat, data=dane, color=kto, zoom=6, maptype="toner-lines",
             size = I(2.5),alpha = I(3/5))
})

output$twitter_mapa_world <- renderPlot({
            dane <- twitter_lon_lat()
            qmplot(lon,lat, data=dane, color=kto, zoom=3, maptype="toner-lines",
                   size=I(2.5), alpha = I(3/5))
      })

  ##### koniec odpowiedzi na kontrolki #####
})

