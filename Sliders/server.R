library(shiny)

shinyServer(function(input, output){
  sliderValues <- reactive({
    data.frame(
      Name = c("Integerfadsfasdasdf", "Decimal", "Range", "Custom Format", "Animation"),
      Value = as.character(c(input$integer,
                             input$decimal,
                             paste(input$range,collapse = '-'),
                             input$format,
                             input$animation)),
      stringsAsFactors = FALSE
    )
  })
  output$values <- renderTable({
    sliderValues()
  })
})