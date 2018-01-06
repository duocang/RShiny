nameOfEmperorList <- c("玉帝老儿")


navbarPage("Superzip", id="nav",
           div(class="outer",
               
               tags$head(
                   # Include our custom css
                   includeScript("gomap.js"),
                   includeCSS("style.css")
               ),
            
               # If not using custom CSS, set height of leafletOutput to a number instead of percent
               leafletOutput("map", width = "100%", height = "100%"),
               
               # Shiny versions prior to 0.11 should use class = "modal" instead.
               absolutePanel(id = "controls", class = "panel panel-defaultd", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             h2("皇帝老儿"),
                             selectInput("nameOfEmperor", "姓名", nameOfEmperorList)
                ),
               tags$div(id="cite",
                        '数据来自维基百科，一会详细写')
               
            ),
           
           tabPanel("数据浏览",
                    fluidRow(
                        column(3,
                               selectInput("name", "姓名", nameOfEmperorList, multiple=TRUE)
                        )
                       
                    ),
                    hr(),
                    DT::dataTableOutput("dataTable")
            )
           
)