# API dla strumienia, nasłuchuje czy określone tweety się pojawiły
require(streamR)
require(ROAuth)

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

data <- strftime(Sys.time(),"%Y-%m-%d@%H:%M")
filterStream( file = file.path("dane","twitter",paste0(data,".json")),
              track = "komorowski,komorowskiego,komorowskiemu,komorowskim,bronkobus,bronkobusem,bronkobusie,bronkobusowie,
              dudy, dudzie, dudę, dudabus, dudabusem,dudabusie,dudabusowi,
                  wanda nowicka, wandy nowickiej, wandzie nowickiej, wandą nowicką, wandzie nowickiej,
              palikot,palikota,palikotowi,palikotem,palikocie,
              korwin,korwina,korwin-mikke,korwin-mikkego,korwinem,
              kukiz,kukiza,kukizowi,kukizem,kukizie,
              magdalena ogórek,magdalenie ogórek, magdaleny ogórek, magdalenie ogórek, magdalenę ogórek, magdaleną ogórek", 
              timeout = 30*60,
              language = "PL",
              #locations = c(14,49,24,54.5),
              oauth=paczka)

# parsedTweets<-parseTweets(file.path("dane","twitter",paste0(data,".json")),simplify=FALSE,verbose=TRUE)
# parsedTweets[,"text"]

