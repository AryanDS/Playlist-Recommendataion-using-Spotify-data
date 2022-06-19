library(shiny )

server <- function(input, output) {
  output$value <- renderText({ input$caption})
  output$value <- renderText({ input$obs })
  output$value <- renderText({ input$obs1 })
}