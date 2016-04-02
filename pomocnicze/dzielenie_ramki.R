#funkcja mając ramkę dla portalu przydziela artykuły do poszczególnych kandydatów
#dostajemy tyle ramek ile kandydatow

podzielRamke <- function(nazwaRamki){
   library(stringi)
   
   ######################################################
   #to nazwiska, które będziemy szukać w artykułach
   kandydat <- c(
      "(Komorowski(\\p{L})*)|((B|b)ronkobus(\\p{L})*)",
      "Marian(\\p{L})* Kowalsk(\\p{L})*",
      "(Dud(\\p{L})*)|((D|d)udabu(\\p{L})*)",
      "Paliko(\\p{L})*",
      "Jarubas(\\p{L})*",
      "Ogórek",
      "Korwin(\\p{L})*",
      "Grodzk(\\p{L})*",
      "Jac(e)*(\\p{L})* Wilk(\\p{L})*",
      "Grzegorz(\\p{L})* Braun(\\p{L})*",
      "Kukiz(\\p{L})*"
   )
   nazwiska <- c("komorowski","kowalski","duda","palikot","jarubas","ogorek","korwin","grodzka",
                 "wilk","braun","kukiz")
   names(kandydat) <- nazwiska
   ######################################################
   ramka <- read.csv(nazwaRamki)
   nazwaKorzen <- stri_extract_first_regex(nazwaRamki, "[^\\.]*")
   for (i in seq_along(nazwiska)){
      #niewrazliwe na wielkosc liter
      regex_kandydata <- paste("(?i)", kandydat[i], sep="")
      #czy w artykule jest wzmianka o kandydacie
      indCzyPisza <- sapply(ramka$tresc, function(x){       #tu byc moze trzeba bedzie zmienic?
         stri_detect_regex(x, regex_kandydata)
      })
      df_kandydata <- ramka[indCzyPisza,]
      row.names(df_kandydata) <- NULL
      if(nrow(df_kandydata)){
         nazwaPliku <- paste(nazwaKorzen, "_", names(kandydat[i]), ".txt", sep="")
         write.csv(df_kandydata, file.path(getwd(), names(kandydat[i]), nazwaPliku), , row.names=FALSE)
      } 
   }
}

setwd("D:/GoogleDrive/Studia/SemestrVIII/R-i-Big-Data/Wybory/dane")
#setwd(file.path(getwd(), "NaTematWybory"))
#setwd(file.path(getwd(), "NaTematGlowna"))
#setwd(file.path(getwd(), "OnetWybory"))
#setwd(file.path(getwd(), "OnetGlowna"))
#setwd(file.path(getwd(), "wPolityce"))

a <- list.files(pattern="[^\\.]*.txt")
n <- length(a)
od <- 146
for (i in od:n){
   podzielRamke(a[i])
}
