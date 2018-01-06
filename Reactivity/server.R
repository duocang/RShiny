library(shiny)
library(datasets)
# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output){
  # By declaring datasetInput as a reactive expression we wnsure that:
  #1) it is only called when the inputs it depends on changes
  #2) the computation and result are shared by all the callers (it
  # only executes a single time)
  #3) when the inputs change and the expression is re-executed,the new
  # result is compared to the previous; if the tow are identical,
  # then the clssers are not notified
  datasetInput <- reactive(
    {
      switch(input$dataset,
             "rock" = rock,
             "pressure" = pressure,
             "cars" = cars)
    }
  )
  
  # the output$caption is computed based on a reactive expression that
  # returns input$caption. When the user chages the "caption" field:
  # 1) this expresion is automatically called to recompute the output
  # 2）the new caption is pushed back to the browser for re-display
  # note that because the data-oriedted reactive expression below don't 
  # depend on input$caption, those expression are not called when
  # input$caption changes.
  output$caption <- renderText(
    {
      input$caption
    }
  )
  output$summary <- renderPrint(
    {
      dataset <- datasetInput()
      summary(dataset)
    }
  )
  output$view <- renderTable(
    {
      head(datasetInput(), n = input$obs)
    }
  )
}
            )