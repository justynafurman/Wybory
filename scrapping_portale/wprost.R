
library(rvest)
library(stringi)
library(tm)
polityka <- "http://www.polityka.pl/TygodnikPolityka"
wprost <- "http://www.wprost.pl/"
html <- html(polityka)

### Główna ###
### do wprostu (naglowek <- html_text(html_nodes(html,".newsbox3_titlearea")))
".newest-list-items"
html_text(html_nodes(html,".md-main-topic_upper-section"))[1]
html_attrs(html_nodes(html,".md-main-topic_upper-section"))[[1]]["href"]
### Obrazki ###
# html_text(html_nodes(html,".md-comments-index_desc")) dno
obrazki <- 
      html_text(html_nodes(html,"h4"))[1:10]
html_attrs(html_nodes(html,"h4"))[1:10]
### Inne ###
html_text(html_nodes(html,"h4"))[-c(1:10)]

### wszystko ###
a <- html_text(html_nodes(html,"a"))
a <- stri_replace_all_regex(a,"\n","")
a <- stri_replace_all_regex(a,"\r","")
a <- stri_replace_all_regex(a,"\t","")
a <- stri_replace_all_regex(a,"(  )*","")
a <- stri_replace_all_regex(a,"skomentuj .+","")
b <- html_attrs(html_nodes(html,"a"))

df <- data.frame(a,sapply(b, function(x) x["href"]))
df$a%in%obrazki

link <- html_attrs(html_nodes(html,".featured a"))[[1]]["href"]
names(link) <- NULL
status <- "glowna"
df <- data.frame(naglowek,status,link)
### Obrazki ###
naglowek <- unique(html_text(html_nodes(html,"#home a")))[-1]
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
      html <- html(paste0(wsieci,link_do_artykulu))
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
    data <- strftime(Sys.time(),"%Y-%m-%d@%H:%M")
    nazwa <- paste(data, paste0("_wsieci_",names(regex_kandydata),".txt"), sep="")
    #zapisuje do pliku txt
    write.csv(dff, file.path("dane","wsieci",names(regex_kandydata),nazwa), row.names=FALSE)
    cat("Znaleziono artykuly dla kandydata ", names(regex_kandydata), "\n")
  }else NULL
}

