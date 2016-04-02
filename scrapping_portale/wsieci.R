przeszukaj_wsieci <- function(){
      library(rvest)
      library(stringi)
      library(tm)
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
      
      cat("\n#################### W S I E C I  ####################\n\n")
      
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
                  write.csv(dff, file.path("dane","wsieci",names(regex_kandydata),nazwa), row.names=FALSE)
                  cat("Znaleziono",length(unique(artykuly)), "artykul(y/ów) dla kandydata", names(regex_kandydata), ".\n")
            }else NULL
      }
}
