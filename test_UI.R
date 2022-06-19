library(shiny)
library(shiny.fluent)
library(dplyr)
library(data.table)
library(lsa)
library(tibble)

data = read.csv('SpotifyFeatures.csv')


Card <- function(..., title = NULL) {
  div(
    class = "ms-depth-8",
    Stack(
      tokens = list(padding = 20, childrenGap=10),
      if (!is.null(title)) {Text(title, variant = "large")},  
      ...
    )
  )
}

Grid <- function(...) {
  div(
    class = 'ms-Grid', dir = "ltr",
    style = "padding: 0px",
    ...
  )
}

GridItem <- function(..., class = "ms-sm12") {
  div(
    class = paste("ms-Grid-col", class),
    style = "padding: 10px",
    ...
  )
}

ui <- fluentPage(
  Grid(
    GridItem(class = "ms-sm6 ms-xl4",
             Card(title = "Inputs",
                  Text(" "),
                  TextField.shinyInput("no_playlist", label="No. of playlists", default=2),
                  TextField.shinyInput("no_song_playlist", label="No. of songs per playlist", default=10),
                  Text(" "),
                  Slider.shinyInput("input1", label="Dance", max=100),
                  Slider.shinyInput("input2", label="Lyrics", max=100),
                  Slider.shinyInput("input3", label="Energy", max=100),
                  Slider.shinyInput("input4", label="Something", max=100),
                  Text(" "),
                  PrimaryButton.shinyInput("submit", text = "Generate Playlists"),
                  DefaultButton.shinyInput("clear", text = "Clear Results", styles = list("background: green"))
                  )
             ),
    GridItem(class = "ms-sm6",
      Card(title = "Outputs", 
           lapply(1:10, function(i) {
             uiOutput(paste0('table', i))
           })
      )
    )
  )
)

server <- function(input, output) {

  observeEvent(input$submit, {
    no_of_playlist = as.integer(input$no_playlist)
    songs_per_playlist = as.integer(input$no_song_playlist)
    total = no_of_playlist * songs_per_playlist
    inputs = c(input$input1,input$input2, input$input3, input$input4)

    final_features = c('popularity', 'danceability', 'acousticness', 'energy')
    new_data = data[,final_features] # Getting our target similarity columns

    similarity <- function(x) {
      return (cosine(x, inputs))
    }

    ss <- apply(new_data, 1, similarity)
    new_data <- cbind(new_data, similar = ss)

    ordered <- new_data[order(-new_data$similar),]
    subset = ordered[1:total,]

    subset <- subset[,final_features]

    # Performing clustering and assigning cluster number to our data
    km.res <- kmeans(subset, no_of_playlist)
    subset <- cbind(subset, cluster = as.vector(km.res$cluster))

    # Splitting the dataset based on cluster number
    X <- split(subset, subset$cluster)

    lapply(1:no_of_playlist, function(i) {
      ff[[paste0('table', i)]] <- renderTable({X[i]})
    })

  })

  observeEvent(input$clear, {
    output$table <- renderUI({

    })
  })

  output$table <- renderUI({
    #DetailsList(items = fluentSalesDeals, columns = columns,
     #           checkboxVisibility = 2)
  })
}

shinyApp(ui, server)

