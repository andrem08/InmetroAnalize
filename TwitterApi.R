library(rtweet)
library(dplyr)
library(stringr)

setwd('C:/Users/andre/PycharmProjects/InmetroAnalize')
getTweets <- function (app, list, n){

  # Cria o token de entrada com as permições de desenvolvedor
  create_token(
    app = app,
    consumer_key = 'hJvDj0D2HKGshjl51phmZGjsd',
    consumer_secret = 'gv4HQkofQk6wJHkpNLpE2OOeMRBZzUSeSNXIV8Lv6kkmtH09Rp',
    access_token = '517626936-3jDJHMiCS8oVanYypvo9b0CzuDk6uwEOLtz3C9OI',
    access_secret = 'jxOjpu5ndJhjrPe6W1g61vQiLIUm005kJoPauZeDlNZLi'
  )

  #Marca o dia atual
  today <- as.POSIXct(as.numeric(Sys.time()), origin = "1970-01-01", tz = 'UTC')
  tweets <- data.frame()
  options(encoding = 'UTF-8')

   for (text in list[[1]]){
     #Modifica o + por OR, para que a pesquisa pprocure por, pelo menos uma das duas palavras separadas por +
     new_text <- str_replace(text, '\\+', 'OR')
     new_tweets <- search_tweets(q = new_text, n = n, lang = 'pt', include_rts = FALSE, type = 'recent')
     new_tweets <- new_tweets$created_at

     #Separa entre dia, semana, mes e o total
     if(!is.null(new_tweets)){
       daily <- sum(as.numeric(today) - as.numeric(new_tweets) < 86400)
       weekly <- sum(as.numeric(today) - as.numeric(new_tweets) < 86400*7)
       month <- sum(as.numeric(today) - as.numeric(new_tweets) < 86400*30)
       year <- sum(as.numeric(today) - as.numeric(new_tweets) < 86400*365)
       all <- length(new_tweets)
     }
     else{
       daily <- 0
       weekly <- 0
       month <- 0
       year <- 0
       all <- 0
     }
     tweets <- rbind(tweets, data.frame(Pesquisa = text, Dia = daily, Semana = weekly))
   }

  return(tweets)
}