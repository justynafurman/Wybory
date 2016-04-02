przeszukaj_wp <- function(){
      require(rvest)
      require(stringi)
      require(tm)
      wp <- html("http://wp.pl")
      
      ### Wybieranie tematów i zapisywanie ich do ramki danych df ###
      
      # STATUS: mainopic
      a <- html_text(html_nodes(wp,".narrowArt")) 
      b <- html_text(html_nodes(wp,".wideArt")) 
      df <- data.frame("status" = "main topic", "tytul" = c(a,b),stringsAsFactors = FALSE)
      # STATUS: newsy
      newsy <- html_text(html_nodes(wp,".txtArts h3"))
      df <- rbind(df, data.frame("status" = "news", "tytul" = newsy,stringsAsFactors = FALSE))
      
      # STATUS: obrazki ze srodka
      inne <- html_text(html_nodes(wp,"h3"))
      inne <- inne[!(inne%in%newsy)]
      df <- rbind(df, data.frame("status" = "obrazki", "tytul" = inne, stringsAsFactors = FALSE))
      
      # STATUS: male obrazki
      inne2 <- html_text(html_nodes(wp,"a h2"))
      inne2 <- inne2[!(inne2%in%df[,2])]
      df <- rbind(df, data.frame("status" = "male obrazki", "tytul" = inne2, stringsAsFactors = FALSE))
      
      wybrane_naglowki <- df
      
      ### ODNOŚNIKI ###
      wszystko <- html_attrs(html_nodes(wp,"a"))
      wszystkie_naglowki <- sapply(wszystko, function(x) x["title"])
      wszystkie_linki <- sapply(wszystko, function(x) x["href"])

      cat("\n#################### Wirtualna Polska  ####################\n\n")
      
      ### WYLAWIANIE ARTYKULOW DLA KANDYDATOW ###
      for( i in seq_along(kandydat)){
            regex_kandydata <- kandydat[i]
            naglowki_z_kandydatem <- wybrane_naglowki[ stri_detect_regex( wybrane_naglowki[,2], regex_kandydata ), ]
           
            # jeśli wyszuka się jakiś naglowek, to....
            if( nrow(naglowki_z_kandydatem) ){
                  # ...szukamy numeru tego artykułu w liście wszystkich, by łatwo wyszukać link
                  nr <- which(wszystkie_naglowki%in%naglowki_z_kandydatem$tytul)
                  linki <- wszystkie_linki[nr]
                  
                  artykuly <- sapply(linki, function(x){
                        # wszystkie linki są postaci: "https://...
                        xx <- html_text(html_nodes(html(x),".ST-Artykul"))
                        if( length(xx) > 0 ){
                              y <- stri_replace_all_regex(xx,"if \\(.+", "")
                              
                              # wstępne oczyszczenie
                              corpus <- Corpus(VectorSource(xx))                       
                              corpus <- tm_map(corpus, stripWhitespace)                          #-usuwanie białych spacji
                              corpus <- tm_map(corpus, removePunctuation)                        #-usuwanie znaków przystankowych
                              corpus <- tm_map(corpus, content_transformer(tolower))             #-transformacja na małe litery                     
                              unlist(sapply(corpus, `[`, "content"))          # sapply zwraca artykul typu character.
                        }else{
                              "brak artykulu" }})
                  
                  dff <-  unique(data.frame("status" = naglowki_z_kandydatem[,1],
                                            "tytul" = naglowki_z_kandydatem[,2],
                                            "artykul" = artykuly,
                                            stringsAsFactors = FALSE ))
                  # czasami z dwóch róznych miejsc pobiera to samo, stąd unique na końcu.
                  # zdarza się, że dany rekord ma ten sam tytuł i treść artykuły, ale w kolumnie czcionka są różne wartości:
                  # tak podobnych rekordów nie usuwam, przydadzą się do oceny widoczności.
                  
                  #nazwa z datą pobierania danych
                  data <- strftime(Sys.time(),"%Y-%m-%d@%H-%M")
                  nazwa <- paste(data, paste0("_wp_",names(regex_kandydata),".txt"), sep="")

                  #zapisuje do pliku txt
                  write.csv(dff, file.path("dane","wp",names(regex_kandydata),nazwa), row.names=FALSE)
                  cat("Znaleziono",length(artykuly), "artykul(y/ów) dla kandydata", names(regex_kandydata), ".\n")
            } else NULL
      }
}