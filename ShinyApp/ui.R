library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Miles Per Gallon"),
  # Siderbar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput("variable", "Variable:",# 用来指定变量
                list("Cylinders" = "cyl",
                     "Transmission" = "am",
                     "Gears" = "gear")),
    checkboxInput("outliers", "Show outliers", FALSE)#用来控制是否显示异常值
  ),
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("mpgPlot")
  )
))