library(shiny)

# shiny服务端的脚本的基本任务是定义输入和输出之间的关系。脚本访问输入值，
# 然后计算，接着向输出的组件赋以反应表达式

# We tweak the "am" field to have nicer factor labels. Since this does not
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output){
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  # Generate a plot of the requested variable against mpg and only
  # include outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = mpgData,
            outline = input$outliers)
  })
})