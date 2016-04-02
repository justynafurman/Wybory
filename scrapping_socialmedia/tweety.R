################################################################################
##   Poniższy skrypt przeznaczony jest do wywolania w konsoli: dzięki temu    ##
##   aplikacja nie blokuje RStudio. Terminal powinien pracować w folderze     ##
##   Wybory. Polecenia do konsoli:                                            ##
##       > R                                                                  ##
##       > load("twitter")                                                    ##                      
##       > dawajtweety( czas_pobierania_w_godzinach )                         ##
##   W czasie działania skrypt poprosi o wprowadzenie klucza autoryzacyjnego. ##
################################################################################

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey    <- "5gUNkfuwFsbRpZ8DWc2sm1er5"
consumerSecret <- "At4GorGYyKJA9ziU9ERjs0viWFLCNU7OssaE2Sd3MeD6Gsqunh"
access_token   <- "2236709701-TjdruvDxyQjrVAc2mSdALKPxBouMGGYgbqexkC3"
access_secret  <- "fNE9QVe7EqdoyccXNob7oan0bfo4YB5HyHltTzvuIrppv"

# proces autoryzacji
paczka <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                           oauthKey = access_token, oauthSecret = access_secret,
                           requestURL = requestURL, accessURL = accessURL, authURL = authURL)
hs <- function(){
      paczka$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
}

dawajtweety <- function( czas_w_godzinach ){
      require(streamR)
      require(ROAuth)
      if( hs() == TRUE){
            data <- strftime(Sys.time(),"%Y-%m-%d@%H-%M")
            filterStream( file = file.path("dane","twitter",paste0(data,".json")),
                          track = "komorowski, komorowskiego, komorowskiemu, komorowskim, bronkobus, bronkobusem, bronkobusie, bronkobusowi,
              dudy, dudzie, dudę, dudabus, dudabusem, dudabusie, dudabusowi,
              palikot, palikota, palikotowi, palikotem, palikocie,
              korwin, korwina, korwin-mikke, korwin-mikkego, korwinem, jkm,
              kukiz, kukiza, kukizowi, kukizem, kukizie,
              magdalena ogórek,magdalenie ogórek, magdaleny ogórek, magdalenie ogórek, magdalenę ogórek, 
              magdaleną ogórek, ogorek,
              grzegorz barun, grzegorza brauna, grzegorzowi braunowi, grzesiowi braunowi, grzegorzem braunem,
              tanajno, tanajnem, tanajnie,
              marian kowalski, mariana kowalskiego, marianowi kowalskiemu, marianem kowalskim, marianie kowalskim,
              jarubas, jarubasa, jarubasem, jarubasowi,
              jacek wilk, jacka wilka, jackowi wilkowi, jackiem wilkiem, jacku wilku", 
                          timeout = czas_w_godzinach*60*60,
                          #locations = c(14,49,24,54.5),
                          oauth=paczka)
      }
     
}

save(requestURL,accessURL,authURL,consumerKey,consumerSecret,access_token,access_secret,
     paczka,dawajtweety,hs,file="twitter")

parsedTweets<-parseTweets(file.path("dane","twitter",paste0(data,".json")), simplify=TRUE,verbose=FALSE)
parsedTweets[,"text"]