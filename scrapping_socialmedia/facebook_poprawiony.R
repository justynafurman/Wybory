########### FUNKCJA POMOCNICZA ########### 
# dopisywanie wyektrahowanego obiektu do istniejacego pliku z odpowiednimi danymi
# co: obiekt
# gdzie: ścieżka

require(stringi)
require(Rfacebook)
load("~/Wybory/fb_oauth")

zapisz <- function( co, gdzie ){
      if( nrow(co)>0 )
            write.table(co, file=gdzie, append=TRUE, col.names = !file.exists(gdzie), row.names = FALSE)
}


########### POSTY I KOMENTARZE ########### 
# funkcja zbiera z publicznych profili danego kandydata jego/o nim posty oraz komentarze pod każdym z nich.
# wynik dla postow i komentarzy zapisywany jest do oddzielnych plików .csv w postaci: kandydat_facebook.csv i kandydat_facebook_comments.csv
fb_posty_i_komentarze <- function( kto, z_ilu_dni_temu=1 ){
      
      # Funkcja dziala dla podanego kandydata.
      # Rdzeniem skryptu sa dwie funkcje pakietu Rfacebook:
      #     getPage() - by wyciagnac posty (z m.in. info o id_postu, tresc_postu)
      #     getPost() - by dla danego id_postu wyciagnac do niego komentarze.
      # Skrypt uruchamiamy 1 raz dziennie, analizujac sytuacje z wczesniejszego dnia.
      # Uruchomienie skryptu dwa razy jednego dnia skutkuje dwukrotnym zapisaniem tych samych informacji
      # i niepotrzebnym zużyciem przestrzeni na dysku.
      # Zakladamy, ze uzytkownicy na biezaco komentuja posty z ostatnich siedmiu dni.
      # Dlatego pobieramy posty z ostatnich 7-miu dni (zapisujac do pliku tylko nowe posty z ostatniego dnia)
      # i dla kazdego z nich spawdzamy, czy wczorajszego dnia pojawil sie jakis nowy komentarz.
      
      ###################### POSTS ###################### 
      wpisy <- data.frame()  # zmienilam nazwe na bardziej (oczywiscie dla mnie tylko) sugestywna.
      strony_kandydata <- id.kandydant.all[ kto ][[1]]  # wszystkie strony o danym kandydacie: [[1]] by odliścić.
      for(page in strony_kandydata){
            wpisy <- rbind( wpisy, tryCatch(
                  getPage(page=page, token=fb_oauth,
                          since = stri_replace_all_regex(as.character( Sys.Date()-(6+z_ilu_dni_temu) ), "-", "/"),
                          until = stri_replace_all_regex(as.character( Sys.Date() ), "-", "/"), 
                          feed=FALSE),
                  error = function(e) return(invisible(NULL))))
      }
      wpisy <- wpisy[!is.na(wpisy$message),]
      wpisy$message <- stri_trim_both(stri_replace_all_regex(wpisy$message,"(\\n)|(\\t)|(\\r)|(\")"," "))
      # posty i komentarze z dnia poprzedniego:
      nowe_wpisy <- wpisy[unlist(stri_extract_all_regex(wpisy$created_time, "[0-9\\-]{10}"))
                          ==(Sys.Date()-z_ilu_dni_temu),]
      # dopisywanie ramki nowych wpisow do pliku danego kandydata:
      sciezka <- paste0("~/Wybory/dane/Facebook/", kto, "_facebook", ".csv")
      zapisz( nowe_wpisy, sciezka )
      
      ###################### COMMENTS ###################### 
      comments <- data.frame()
      # jadąc po id każdego wpisu z ostatniego tygodnia, zbieramy do niego NOWE komentarze
      for( id_wpisu in wpisy$id ){
            gotten_comments <- getPost(id_wpisu, token=fb_oauth, likes = FALSE)[2]
            # getPost ma dwa elementy: 1: treść postu, 2: komentarze na jego temat.
            # ponieważ treść już mamy, pierwszy element wydaje się niepotrzebny.
            tmp_comments<-as.data.frame(gotten_comments)
            # zapiszemy komentarze bez znaków specjalnych i niepotrzebnych spacji na końcach.
            tmp_comments$comments.message <- stri_trim_both(stri_replace_all_regex(tmp_comments$comments.message,"(\\n)|(\\t)|(\\r)|(\")"," ")                        )
            # (NOWE) komentarze z dnia poprzedniego
            nowe_komentarze<-tmp_comments[unlist(stri_extract_all_regex(tmp_comments$comments.created_time,
                                                                        "[0-9\\-]{10}"))==(Sys.Date()-z_ilu_dni_temu),]
            tresc_wpisu <- wpisy$message[wpisy$id==id_wpisu]
            m <- merge(tresc_wpisu, nowe_komentarze)
            comments <- rbind(comments, m)
      }
      sciezka <- paste0("~/Wybory/dane/Facebook/", kto, "_facebook_comments", ".csv")
      zapisz( comments, sciezka )
}

########### LAJKI ########### 
# funkcja ktora zapisuje liczbe likeow dla stron na temat kandydatow na prezydenta
fb_likes<-function( kto ){
      # zawiera rowniez uzywany pozniej obiekt: id.kandydant.all  # contains fb_oauth data and id.kandydat.all as well.
      candidate_page_id <- id.kandydant.all[ kto ][[1]]
      likes<-getUsers(users = candidate_page_id, token=fb_oauth)
      likes<-likes[, c(1, 2, 9, 10)]  # extracting cols from the likes_df: id & name (of page), n_of_likes, pics.
      likes<-cbind(likes, date=Sys.Date())  # when does this happen?
      # write it down here: kandydant.name_likes
      sciezka <- paste0("~/Wybory/dane/Facebook/Likes/", kto, "_likes", ".csv")
      zapisz( likes, sciezka )
}

########### DODATKOWA FUNKCJA ###########
# Funkcja automatyzujaca dzialanie. Jej wywolanie pracuje na dwoch powyzszych implementacjach.
# Jesli nie moglismy zrobic danej sesji facebookowej danego dnia mozemy wywolac ja z dodatkowym
# parametrem, okreslajacym za ktory dzien ma byc sesja wykonana. Domyslna wartosc to: wczoraj.
# Zrobilam bo fajnie sie ja sama puszcza z konsoli :P

fb_sesja <- function(z_ilu_dni_temu=1){
      for( kto in names(id.kandydant.all) ){
            cat("\n", kto, " posty: \n")
            fb_posty_i_komentarze( kto, z_ilu_dni_temu )
            fb_likes( kto )
      }
}

# fb_sesja()

#crontab command: 
# 01 21 * 4,5  * Rscript ~/Wybory/facebook_poprawiony.R  >> /home/katarzyna/Wybory/raport_fb.log

## Nadrobienie sesji wcześniejszych:

cat(paste("\n start at:",Sys.time()))
for(dni in 1:16){
      cat(dni, "dni temu")
      fb_sesja( dni )
}
cat(paste(Sys.time(),"fb_sesja done. possible errors... ? \n", getwd(),"\n"))
