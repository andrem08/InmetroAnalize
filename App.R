library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(ggvis)
library(ggplot2)
source('TwitterApi.R')
source('YoutubeApi.R')
app_name <- 'InmetroAnalize'
options(encoding = 'UTF-8')

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
      column(2,
        selectInput('select_period', 'Selecione o período', choices = c('Dia', 'Semana')),
        numericInput('numeric_input', 'Max de pesquisas:', 100, 1, 1000),
        actionButton('load_graph', 'Carregar grafico'),
      ),
      column(10,
        plotOutput('gggraph')
      )
    )
  )

  observeEvent(input$load_graph, {

    #Pegando os dados das redes sociais
    textList <- read.delim('palavras_chaves')
    tweets_stats <- getTweets(app_name, as.list(textList), 100)
    # yt_stats <- getYtSearch(as.list(textList), 10)
    yt_stats <- getYtSearch(as.list(textList), 10)

    output$tweets_table <- renderDataTable(tweets_stats)
    output$yt_table <- renderDataTable(yt_stats)

    #Construindo o gráfico

    # visPlot(tweets_stats, yt_stats, inpur$select_period)
    output$gggraph <- renderPlot(ggPlotF(tweets_stats, yt_stats, input$select_period))
  })
}
ggPlotF <- function (tweets_stats, yt_stats, period){
  tweets_stats <- data.frame(tweets_stats, rede_social = 'Twitter')
  yt_stats <- data.frame(yt_stats, rede_social = 'Youtube')

  data <- rbind(tweets_stats, yt_stats)
  ggGraph <- ggplot(data = data, aes(x = Pesquisa, y = Semana, colour = rede_social, shape = rede_social)) +
    geom_point(size = 5, alpha = 0.7) +
    labs(title = "Grafico entre redes sociais:\n") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  return(ggGraph)

}

visPlot <- function (tweets_stats, yt_stats, period){
    vis <- ggvis(data = NULL, x = ~Pesquisa, y = ~Semana)%>%
      layer_points(fill := "red", data = tweets_stats) %>%
      layer_points(fill := 'blue', data = yt_stats) %>%
      add_axis('x', title = 'Palavras chaves') %>%
      add_axis('y', title = 'Frequencia') %>%
      scale_nominal("stroke", c('Twitter', 'Youtube'), range = c('red', 'blue')) %>%
      bind_shiny('graph_table', 'ggvis_ui')

  vis <- ggvis(data = cars,x = ~speed, y = ~dist )%>%
        layer_points(fill := "red")
  bind_shiny(vis, 'graph_table')
}

shinyApp(ui =  ui, server = server)