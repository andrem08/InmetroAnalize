library(tuber)
library(stringr)


getYtSearch <- function (list, n){

  # Cria o token de entrada com as permições de desenvolvedor
  yt_oauth('401354068272-2m6d5bede2an0kme52osd76ongo27vj8.apps.googleusercontent.com', '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')

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