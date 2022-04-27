library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
source('TwitterApi.R')
source('YoutubeApi.R')
app_name <- 'InmetroAnalize'

ui <-
  fluidPage(
    theme = (theme = 'sandstone'),
    setBackgroundColor(),
    navbarPage((''),
      tabPanel('Pesquisa do Inmetro.',
          tabsetPanel(type = 'tabs', id = 'user_heart_pa_summary',
                      tabPanel(title = 'Grafico comparando as redes sociais', uiOutput('graph')),
                      tabPanel(title = 'Tabela do Inmetro - Twitter', shinycssloaders::withSpinner(dataTableOutput('tweets_table'))),
                      tabPanel(title = 'Tabela do Inmetro - Youtube', shinycssloaders::withSpinner(dataTableOutput('yt_table'))),
          )
      ),
    )
)

server <- function (input, output, session){

  output$graph <- renderUI(
    tagList(
        selectInput('select_period', 'Selecione o período', choices = c('Dia', 'Semana')),
        actionButton('load_graph', 'Carregar grafico'),
        ggvisOutput('graph_table')
    )
  )

  observeEvent(input$load_graph, {

    #Pegando os dados das redes sociais
    textList <- read.delim('palavras_chaves')
    tweets_stats <- getTweets(app_name, as.list(textList), 10)
    yt_stats <- getYtSearch(as.list(textList), 10)

    output$tweets_table <- renderDataTable(tweets_stats)
    output$yt_table <- renderDataTable(yt_stats)

    #Construindo o gráfico

    vis <- visPlot(tweets_stats, yt_stats, inpur$select_period)

    bind_shiny(vis, 'graph_table')

  })
}

visPlot <- function (tweets_stats, yt_stats, period){
    vis <- ggvis(data = NULL, x = ~text, y = ~period)%>%
      layer_points(fill := "red", data = tweets_stats) %>%
      layer_points(fill = 'blue', data = yt_stats) %>%
      add_axis('x', title = 'Interval of years') %>%
      add_axis('y', title = 'Heart rate') %>%
      scale_nominal("stroke", c('Twitter', 'Youtube'), range = c('red', 'blue'))

  return (vis)
}

shinyApp(ui =  ui, server = server)