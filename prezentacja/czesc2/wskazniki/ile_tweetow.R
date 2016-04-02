ile_tweetow <- function(){
      nazwy <- c("komorowski","duda","kukiz","ogorek","palikot","wilk","korwin","braun","kowalski","jarubas")
      liczba_tweetow <- numeric(length(nazwy))
      
      for(i in seq_along(nazwy)){
            kandydat <- read.csv(paste0("dane/twitter_rozdzielone/",nazwy[i]))
            liczba_tweetow[i] <- nrow(unique(kandydat["text"]))
      }
      
      barplot(liczba_tweetow,names.arg=nazwy,las=2)
}
