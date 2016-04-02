library(twitteR)
library(streamR)
df <- data.frame()

for( each_json in list.files("dane/twitter/")){
      df <- rbind(df, parseTweets(file.path("dane","twitter",each_json), simplify=TRUE,verbose=FALSE))
}

regeksy <- c("((K|k)omorowsk)|(bronkobus)","(D|d)ud","(K|k)ukiz","(O|o)g(o|ó)rek", "(P|p)alikot",
             "(W|w)ilk","((K|k)orwin)|(JKM)|(jkm)","(B|b)raun","(T|t)anajn","(K|k)owalski","(J|j)aruba")
nazwy <- c("komorowski","duda","kukiz","ogorek","palikot","wilk","korwin","braun","kowalski","jarubas")

for(reg in 1:length(regeksy)){
      nazwa <- nazwy[reg]
      dd <- df[sapply(df["text"], function(x) stri_detect_regex(x, regeksy[reg])),]
      write.csv(dd, file=file.path("dane","twitter_rozdzielone",nazwa))
}
