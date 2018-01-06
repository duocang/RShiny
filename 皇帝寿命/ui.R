
dynastyList <- unique(daynstyInfo$时代)

navbarPage("大统领和小统领", id="nav",
    tabPanel("交互地图",
        div(class="outer",
         
             tags$head(
                 # Include our custom css
                 includeScript("gomap.js"),
                 includeCSS("style.css")
             ),
            
            birthPlaceInput("birthplace")
        )
    )
    ,           
           
    tabPanel("数据浏览",
            fluidRow(
                column(3,
                       selectInput("name", "姓名", emperorList, multiple=TRUE)
                )
               
            ),
            hr(),
            DT::dataTableOutput("dataTable")
    )
   
)