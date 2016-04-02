przeszukaj_interia <- function(){
      require(rvest)
      require(stringi)
      require(tm)
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
      
      cat("\n\n#################### I N T E R I A  ####################\n\n")
      
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
                  write.csv(dff, file.path("dane","interia",names(regex_kandydata),nazwa), row.names=FALSE)
                  cat("Znaleziono",length(unique(artykuly)), "artykul(y/ów) dla kandydata", names(regex_kandydata), ".\n")
            }else NULL
      }
}