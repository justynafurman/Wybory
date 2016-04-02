### SKRYPT DO ZADAN DLA crontaba ###
### wyniki w pliku: raport.log ###
######################################################################################################
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
      "Kukiz(\\p{L})*",
      "Nowick(\\p{L})*",
      "Tanajn(\\p{L})*"
)
nazwiska <- c("komorowski","kowalski","duda","palikot","jarubas","ogorek","korwin","grodzka",
              "wilk","braun","kukiz","nowicka","tanajno")
names(kandydat) <- nazwiska

cat("\n ************ ",strftime(Sys.time(),"%Y-%m-%d@%H-%M"), " ************")


      cat("\nTVN24:\n")
      require(rvest)
      require(stringi)
      require(tm)
      tvn24 <- html("http://tvn24.pl")      
      
      ### Wybieranie tematów i zapisywanie ich do ramki danych df ###
      
      ### NEWSY
      status <- "newsy"
      czcionka <- 10
      naglowek <- html_text(html_nodes(tvn24,"#newestNewsList a"))
      link <- html_attrs(html_nodes(tvn24,"#newestNewsList a"))
      link <- sapply( link, function(x) x["href"] )
      names(link) <- NULL
      df <- data.frame( status, czcionka, naglowek, link )
      
      ### MAINTOPIC
      status <- "maintopic"
      naglowek <- html_text(html_nodes(tvn24,".myCenter"))
      attrs <-  html_attrs(html_nodes(tvn24,".myCenter a"))
      link <- sapply( attrs, function(x) x["href"] )
      czcionka <- sapply( attrs, function(x) x["class"] )
      names(link) <- NULL; names(czcionka) <- NULL;
      df <- rbind( df, data.frame( status, czcionka, naglowek, link ) )
      
      ### OBRAZKI, tu będzie więcej kłopotów... wszystko po to, żeby artykuly się nie dublowały.
      status <- "obrazki"
      attrs <- html_attrs(html_nodes(tvn24,".mainLeftColumn a"))
      naglowek <- t <- sapply( attrs, function(x) if( is.na(x["title"]) ) " " else x["title"] )
      
      # nagłówki są zdublowane, unique temu nie zapobiegnie, trzeba usunąć niepotrzebne frazy:
      t <- stri_replace_all_regex( t, "^(O|o)dtwórz: ","")
      t <- stri_replace_all_regex( t, "^(zobacz więcej )","")
      t <- stri_replace_all_regex( t, "^(zobacz więcej: )","")
      t <- stri_replace_all_regex( t, "^(czytaj dalej o: )","")
      t <- stri_replace_all_regex( t, "^(czytaj dalej )","")
      t <- stri_replace_all_regex( t, "( )+$","")
      t <- stri_replace_all_regex( t, "^ ","")      
      naglowek <- t
      
      link <- t <- sapply( attrs, function(x) if( is.na(x["href"]) ) " " else x["href"] )
      # niektore linki sie powtarzaja z kosmetyczna roznica.
      t <- stri_replace_all_regex( t, "(#autoplay)$","")
      t <- stri_replace_all_regex( t, "(#forum)$","")
      t <- stri_replace_all_regex( t, " $","")
      t <- stri_replace_all_regex( t, "^ ","")
      link <- t
      names(link) <- NULL; names(naglowek) <- NULL;
      czcionka <- 0
      df_temp <- data.frame( status, czcionka, naglowek, link )
      df_temp <-df_temp[!duplicated(df_temp),]
      df <- rbind( df, df_temp )
      
      ### WYSZUKANIE ROZMIARÓW TYTUŁÓW ###
      # nagłówki po rozmiarach
      fonts <- seq(from=10, to=70, by=2)
      size <- vector("list",length(fonts))
      df_size <- data.frame(size=0,tytul="0")
      for( i in seq_along(size) ){
            if( length( html_text(html_nodes( tvn24, paste0(".size",fonts[i]) )) ) )
                  df_size <- rbind(df_size, data.frame(size=fonts[i], tytul = html_text(html_nodes(tvn24,paste0(".size",fonts[i])))
                  )) }
      
      for( i in seq_along(df_size[,1]) )
            df$czcionka[ which( df$naglowek==as.character(df_size$tytul[i]) ) ]  <-  df_size$size[i]
      
      # uwaga! przy newsach powinna być domyslna czcionka = 10.
      # jeśli tak nie jest, możliwe, że ten sam artykul pojawił się w innym miejscu na stronie
      # w większym formacie. ta pozorna niścisłośc może być cenna informacją w przyszłości.
      
      for( i in seq_along(kandydat)){
            regex_kandydata <- kandydat[i]
            df_kandydata <- df[(stri_detect_regex( df$naglowek, regex_kandydata ) ), ]
            if( nrow(df_kandydata) )
            {
                  linki_do_artykulu <- df_kandydata$link
                  # czyszczenie tytulu
                  tytuly <- stri_replace_all_regex(df_kandydata$naglowek,"\n","")
                  tytuly <- stri_replace_all_regex(tytuly,"\r","")
                  artykuly <- sapply(linki_do_artykulu, function(x){
                        if( stri_detect_regex(x,"http") ){
                              xx <- html_text(html_nodes(html(as.character(x)),"div article"))[1]
                              if( length(xx) > 0 ){
                                    # wydłubanie tekstu
                                    y <- stri_replace_all_regex(xx,"Autor: .+","")
                                    y <- stri_replace_all_regex(y,"Źródło: .+","")
                                    y <- stri_replace_all_regex(y,"if \\(.+","")
                                    y <- stri_replace_all_regex(y,"video.+","")
                                    # wstępne oczyszczenie
                                    corpus <- Corpus(VectorSource(y))                       
                                    corpus <- tm_map(corpus, stripWhitespace)                          #-usuwanie białych spacji
                                    corpus <- tm_map(corpus, removePunctuation)                        #-usuwanie znaków przystankowych
                                    corpus <- tm_map(corpus, content_transformer(tolower))             #-transformacja na małe litery
                                    unlist(sapply(corpus, `[`, "content"))
                              }else{"brak artykulu"}
                        }else{
                              xx <- html_text(html_nodes(html(paste0("http://tvn24.pl",x)),"div article"))[1]
                              if( length(xx) > 0 ){
                                    # wydlubanie tekstu
                                    y <- stri_replace_all_regex(xx,"Autor: .+","")
                                    y <- stri_replace_all_regex(y,"Źródło: .+","")
                                    y <- stri_replace_all_regex(y,"if \\(.+","")
                                    y <- stri_replace_all_regex(y,"video.+","")
                                    # wstepne oczyszczenie
                                    corpus <- Corpus(VectorSource(y))                       
                                    corpus <- tm_map(corpus, stripWhitespace)                          #-usuwanie białych spacji
                                    corpus <- tm_map(corpus, removePunctuation)                        #-usuwanie znaków przystankowych
                                    corpus <- tm_map(corpus, content_transformer(tolower))             #-transformacja na małe litery
                                    unlist(sapply(corpus, `[`, "content"))
                              }else{"brak artykulu"}
                        }
                  }) # koniec sapply-ja dla artykuly()

                  dff <- unique(data.frame("status" = df_kandydata$status,
                                           "czcionka" = df_kandydata$czcionka,
                                           "tytul" = tytuly,
                                           "artykul" = artykuly,
                                           stringsAsFactors = FALSE ))
                  data <- strftime(Sys.time(),"%Y-%m-%d@%H-%M")
                  nazwa <- paste(data, paste0("_tvn24_",names(regex_kandydata),".txt"), sep="")
                  #zapisuje do pliku txt
                  write.csv(dff, file.path("~","Wybory","dane","tvn24",names(regex_kandydata),nazwa), row.names=FALSE)
                  cat("Znaleziono",length(unique(artykuly)), "artykul(y/ów) dla kandydata", names(regex_kandydata), ".\n")
            }else NULL
      }



      cat("\nINTERIA:\n")
      html<-html("http://interia.pl/")
      
      ### MAIN TOPIC ###
      status <- "duży news"
      naglowek <- html_text(html_nodes(html,".news-one-a"))
      link <- html_attrs(html_nodes(html,".news-one-a"))
      link <- sapply( link, function(x){ x["href"] } )
      names(link) <- NULL
      df <- data.frame( status, naglowek, link )
      ### OBRAZKI DUŻE ###
      status <- "obrazki duze"
      naglowek <- sapply( html_attrs(html_nodes(html,".tiles-a")), function(x) x["title"] )
      link <- sapply( html_attrs(html_nodes(html,".tiles-a")), function(x) x["href"] )
      names(naglowek) <- NULL; names(link) <- NULL;
      df <- rbind( df, data.frame(status, naglowek, link) )
      ### NEWSY ###
      status <- "newsy"
      naglowek <- sapply( html_attrs(html_nodes(html,".news-a")), function(x) x["title"] )
      link <- sapply( html_attrs(html_nodes(html,".news-a")), function(x) x["href"] )
      names(naglowek) <- NULL; names(link) <- NULL;
      df <- rbind( df, data.frame(status, naglowek, link) )
      
      
      for( i in seq_along(kandydat)){
            regex_kandydata <- kandydat[i]
            df_kandydata <- df[(stri_detect_regex( df$naglowek, regex_kandydata ) ), ]
            if( nrow(df_kandydata) )
            {
                  linki_do_artykulu <- df_kandydata$link
                  artykuly <- sapply( linki_do_artykulu, function(x){
                        y <- html_text(html_nodes(html(as.character(x)),".main-content p"))
                        y <- stri_flatten(y)
                        if( length(y)> 0){
                              # wstępne oczyszczenie
                              corpus <- Corpus(VectorSource(y))
                              corpus <- tm_map(corpus, stripWhitespace) #-usuwanie białych spacji
                              corpus <- tm_map(corpus, removePunctuation) #-usuwanie znaków przystankowych
                              corpus <- tm_map(corpus, content_transformer(tolower)) #-transformacja na małe litery
                              unlist(sapply(corpus, `[`, "content"))
                        }else{"brak artykulu"}    
                  })
                  
                  dff <- unique(data.frame("status" = df_kandydata$status,
                                           "tytul" = df_kandydata$naglowek,
                                           "artykul" = artykuly,
                                           stringsAsFactors = FALSE ))
                  data <- strftime(Sys.time(),"%Y-%m-%d@%H-%M")
                  nazwa <- paste(data, paste0("_interia_",names(regex_kandydata),".txt"), sep="")
                  #zapisuje do pliku txt
                  write.csv(dff, file.path("~","Wybory","dane","interia",names(regex_kandydata),nazwa), row.names=FALSE)
                  cat("Znaleziono",length(unique(artykuly)), "artykul(y/ów) dla kandydata", names(regex_kandydata), ".\n")
            }else NULL
      }


      cat("\nwSIECI:\n")
      wsieci <- "http://www.wsieci.pl/"
      html <- html(wsieci)
      
      ### NAGLOWEK ###
      naglowek <- html_text(html_nodes(html,".right"))[2]
      link <- html_attrs(html_nodes(html,".featured a"))[[1]]["href"]
      names(link) <- NULL
      status <- "glowna"
      df <- data.frame(naglowek,status,link)
      ### Obrazki ###
      naglowek <- unique(html_text(html_nodes(html,"#home a")))[-1]
      h <- unique(html_attrs(html_nodes(html,"#home a")))
      link <- unique(sapply(h, function(x) x["href"]))
      status <- "obrazki"
      df <- rbind(df,data.frame(naglowek,status,link))
      
      for( i in seq_along(kandydat)){
            regex_kandydata <- kandydat[i]
            df_kandydata <- df[(stri_detect_regex( df$naglowek, regex_kandydata ) ), ]
            if( nrow(df_kandydata) )
            {
                  linki_do_artykulu <- df_kandydata$link
                  # czyszczenie tytulu
                  tytuly <- stri_replace_all_regex(df_kandydata$naglowek,"(  )+","")
                  tytuly <- stri_replace_all_regex(tytuly,"\n","")
                  tytuly <- stri_replace_all_regex(tytuly,"\r","")
                  artykuly <- sapply(linki_do_artykulu, function(x){
                        html <- html(paste0(wsieci,x))
                        y <- html_text(html_nodes(html,"#contentBody"))
                        if( length(y)> 0){
                              # wstępne oczyszczenie
                              corpus <- Corpus(VectorSource(y))
                              corpus <- tm_map(corpus, stripWhitespace) #-usuwanie białych spacji
                              corpus <- tm_map(corpus, removePunctuation) #-usuwanie znaków przystankowych
                              corpus <- tm_map(corpus, content_transformer(tolower)) #-transformacja na małe litery
                              unlist(sapply(corpus, `[`, "content"))
                        }else{"brak artykulu"}    
                  })
                  
                  dff <- unique(data.frame("status" = df_kandydata$status,
                                           "tytul" = tytuly,
                                           "artykul" = artykuly,
                                           stringsAsFactors = FALSE ))
                  data <- strftime(Sys.time(),"%Y-%m-%d@%H-%M")
                  nazwa <- paste(data, paste0("_wsieci_",names(regex_kandydata),".txt"), sep="")
                  #zapisuje do pliku txt
                  write.csv(dff, file.path("~","Wybory","dane","wsieci",names(regex_kandydata),nazwa), row.names=FALSE)
                  cat("Znaleziono",length(unique(artykuly)), "artykul(y/ów) dla kandydata", names(regex_kandydata), ".\n")
            }else NULL
      }

      cat("\nWP:\n")
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
                  write.csv(dff, file.path("~","Wybory","dane","wp",names(regex_kandydata),nazwa), row.names=FALSE)
                  cat("Znaleziono",length(artykuly), "artykul(y/ów) dla kandydata", names(regex_kandydata), ".\n")
            } else NULL
      }

cat("\n\n")
