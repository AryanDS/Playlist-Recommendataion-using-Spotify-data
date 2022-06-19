#Login page UI
library("shiny")


ui <- fluidPage(
  textInput("caption", "User Name", ""),
  verbatimTextOutput("value"),
  
  numericInput("obs", "Number of Playlist:", "",min = 1, max = 10),
  verbatimTextOutput("value"),
  
  numericInput("obs1", "Number of Songs in the Playlist:","", min = 1, max = 100),
  verbatimTextOutput("value")
)
