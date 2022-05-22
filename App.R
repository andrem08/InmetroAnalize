#Bibliotecas utilizadas no programa

library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(ggvis)
library(ggplot2)
source('TwitterApi.R')
source('YoutubeApi.R')

#Nome do aplicativo
app_name <- 'InmetroAnalize'

#Codificação para os açentos
options(encoding = 'UTF-8')
options(repr.plot.width=15, repr.plot.height=18)

#interface gráfica
ui <-
  fluidPage(
    theme = (theme = 'sandstone'),
    setBackgroundColor(),
    navbarPage((''),
               tabPanel('Pesquisa do Inmetro.',
                        tabsetPanel(type = 'tabs', id = 'user_heart_pa_summary',
                                    tabPanel(title = 'Gráfico comparando as redes sociais', uiOutput('graph')),
                                    tabPanel(title = 'Lista de termos e palavras chaves', shinycssloaders::withSpinner(dataTableOutput('keywords'))),
                                    tabPanel(title = 'Tabela do Inmetro - Twitter', shinycssloaders::withSpinner(dataTableOutput('tweets_table'))),
                                    tabPanel(title = 'Tabela do Inmetro - Youtube', shinycssloaders::withSpinner(dataTableOutput('yt_table'))),

          )
               )
    )
  )

server <- function (input, output, session){

  today <- as.POSIXct(as.numeric(Sys.time()), origin = "1970-01-01", tz = 'UTC')

  output$graph <- renderUI(
    tagList(
      column(2,
             wellPanel(
               h4(strong('Selecione o período de tempo, até a data: ')),
               h4(today),
               selectInput('select_period', '', choices = c('Um dia atrás', 'Uma semana atrás')),
               numericInput('numeric_input', 'Max de pesquisas: (Limite de 100)', 10, 1, 100),
               actionButton('load_graph', 'Carregar grafico'),
             )

      ),
      column(10,
             plotOutput('gggraph'),
             dataTableOutput('keywords_table')
      )
    )
  )

  observeEvent(input$load_graph, {

    #texto contendo as palavras chaves
    textList <- read.delim('palavras_chaves')

    #Funções que obtem os data frames de cada uma das redes sociais
    tweets_stats <- getTweets(app_name, as.list(textList), input$numeric_input)
    yt_stats <- getYtSearch(as.list(textList), numeric_input)
    yt_stats <- getYtSearch2(tweets_stats)

    #Utilizando a função para construção do gráfico e renderizando-o
    ggPlotF(tweets_stats, yt_stats, input$select_period, output)
  })
}

#Função que, recebe tanto os data frames dos resultados da pesquisa do twitter quanto do youtube
ggPlotF <- function (tweets_stats, yt_stats, period, output){

  pesquisa <- as.list(read.delim('palavras_chaves'))
  names(pesquisa) <- 'Pesquisa'

  #Caso alguma das funções retorne com erro
  if (is.null(yt_stats) ){
    yt_stats <- data.frame(Pesquisa = pesquisa, Dia = 0, Semana = 0, rede_social = 'Youtube')
  }else{
    yt_stats <- data.frame(yt_stats, rede_social = 'Youtube')
  }

  if (is.null(tweets_stats) ){
    tweets_stats <- data.frame(Pesquisa = pesquisa, Dia = 0, Semana = 0, rede_social = 'Twitter')
  }else{
    tweets_stats <- data.frame(tweets_stats, rede_social = 'Twitter')
  }

  #União dos data frames e construção do gráfico
  #Cores e formatos determinados de acordo com a rede social
  data <- rbind(tweets_stats, yt_stats)
  if( period == 'Uma semana atrás'){
    ggGraph <- ggplot(data = data, aes(x = Pesquisa, y = Semana, colour = rede_social, shape = rede_social)) +
      geom_point(size = 5, alpha = 0.7) +
      labs(title = "Gráfico comparando as frequências das palavras chaves de cada uma redes sociais, na última semana:\n", y = 'Frequências') +
      theme(plot.title = element_text(size = 19), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12))

  }
  if( period == 'Um dia atrás'){
    ggGraph <- ggplot(data = data, aes(x = Pesquisa, y = Dia, colour = rede_social, shape = rede_social)) +
      geom_point(size = 5, alpha = 0.7) +
      labs(title = "Gráfico comparando as frequências das palavras chaves de cada uma redes sociais, no último dia:\n", y = 'Frequências') +
      theme(plot.title = element_text(size = 19), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12))
  }


  #Retorna o gráfico desejado
  output$gggraph <- renderPlot(ggGraph)
  output$tweets_table <- renderDataTable(tweets_stats)
  output$yt_table <- renderDataTable(yt_stats)
  output$keywords <- renderDataTable(data.frame(Índice = seq(length(pesquisa$Pesquisa)), Pesquisa = pesquisa$Pesquisa[order(pesquisa$Pesquisa)]))

}

#Inicializa o aplicativo
shinyApp(ui =  ui, server = server)