przeszukaj_tvn24 <- function(){
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
      cat("\n#################### T V N 24  ####################\n\n")
      
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
                  write.csv(dff, file.path("dane","tvn24",names(regex_kandydata),nazwa), row.names=FALSE)
                  cat("Znaleziono",length(unique(artykuly)), "artykul(y/ów) dla kandydata", names(regex_kandydata), ".\n")
            }else NULL
      }
}