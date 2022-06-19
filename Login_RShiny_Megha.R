library("shiny")
# ui <- fluidPage(
#   
#   # Copy the line below to make a text input box
#   textInput("text", label = h3("Please enter your Name"), value = ""),
#   
#   hr(),
#   fluidRow(column(3, verbatimTextOutput("value")))
#   
# )
# server <- function(input, output) {
#   
#   # You can access the value of the widget with input$text, e.g.
#   output$value <- renderPrint({ input$text })
#   
# }

ui <- fluidPage(
  textInput("caption", "User Name", ""),
  verbatimTextOutput("value"),
  
  numericInput("obs", "Number of Playlist:", "",min = 1, max = 10),
  verbatimTextOutput("value"),
  
  numericInput("obs1", "Number of Songs in the Playlist:","", min = 1, max = 100),
  verbatimTextOutput("value")
)
server <- function(input, output) {
  output$value <- renderText({ input$caption})
  output$value <- renderText({ input$obs })
  output$value <- renderText({ input$obs1 })
}

shinyApp(ui, server)
