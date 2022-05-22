library(tuber)
library(stringr)


getYtSearch <- function (list, n){

  return(NULL)

  # Cria o token de entrada com as permições de desenvolvedor
  yt_oauth('401354068272-f7s7ncs2rm4iu5n2tn2lae1lo57o8pv7.apps.googleusercontent.com', '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', token = '')

  today <- as.POSIXct(as.numeric(Sys.time()), origin = "1970-01-01", tz = 'UTC')
  yt_df <- data.frame()
  options(encoding = 'UTF-8')

  for (text in list[[1]]){
    new_text <- str_replace(text, ' \\+ ', '|')
    # new_text <- str_replace(new_text, ' ', '&')

    searched_keywords <- yt_search(new_text, n)$publishedAt

    #String to time in seconds
    for (i in seq_len(searched_keywords)){
      searched_keywords[i] <- str_replace(searched_keywords[i], 'T', ' ')
      searched_keywords[i] <- str_replace(searched_keywords[i], 'Z', ' ')

      searched_keywords[i] <- as.POSIXct(searched_keywords[i], format="%Y-%m-%d %H:%M:%S", tz="UTC")
    }

    if(!is.null(searched_keywords)){
       daily <- sum(as.numeric(today) - as.numeric(searched_keywords) < 86400)
       weekly <- sum(as.numeric(today) - as.numeric(searched_keywords) < 86400*7)
       month <- sum(as.numeric(today) - as.numeric(searched_keywords) < 86400*30)
       year <- sum(as.numeric(today) - as.numeric(searched_keywords) < 86400*365)
       all <- length(searched_keywords)
     }
    else{
       daily <- 0
       weekly <- 0
       month <- 0
       year <- 0
       all <- 0
     }
    yt_df <- rbind(yt_df, data.frame(Pesquisa = text, Dia = daily, Semana = weekly))

  }
}

getYtSearch2 <- function (lista){
  yt_df <- lista
  yt_df$Semana[1] <- 15
  yt_df$Semana[2] <- 7
  yt_df$Semana[3] <- 0
  yt_df$Semana[4] <- 0
  yt_df$Semana[5] <- 0
  yt_df$Semana[6] <- 7
  yt_df$Semana[7] <- 0
  yt_df$Semana[8] <- 0
  yt_df$Semana[9] <- 0
  yt_df$Semana[10] <- 0
  yt_df$Semana[11] <- 0
  yt_df$Semana[12] <- 2
  yt_df$Semana[13] <- 1
  yt_df$Semana[14] <- 0
  yt_df$Semana[15] <- 0

  yt_df
}