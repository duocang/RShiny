test_title="Decison Support System"

test_system<-list(
)

test_side=list(
    sidebarMenu(id = "tabs",
                menuItem("File Upload", icon = icon("upload"),
                         div(id="rad",
                             fileInput('file1', 'Choose file to upload',
                                       accept = c(
                                           'text/csv',
                                           'text/comma-separated-values',
                                           'text/tab-separated-values',
                                           'text/plain',
                                           '.csv',
                                           '.tsv'
                                       )
                             ))
                ),
                menuItem("Show The Seperators", icon = icon("drupal"), tabName = "dashboard",
                         div(id="rad",radioButtons('sep', 'Separator',
                                                   c(Comma=',',
                                                     Semicolon=';',
                                                     Tab='\t'),
                                                   ','))),
                menuItem("Show The Variables", icon = icon("codepen"), tabName = "widgets",
                         div(id="rad",selectInput('xcol', 'X Variable',""),
                             selectInput('ycol', 'Y Variable',""),
                             selectInput('zcol', 'Z Variable',""))
                ),
                tags$style(type="text/css", "#rad {background-color:  #ffffff;color: #000000;
                           }")
  )
  
    )

test_main=list(
    navbarPage(title=div(icon("jsfiddle"),"Business Intelligence"),
               tabPanel("Dashboard", icon = icon("dashboard")),
               tabPanel("Data", icon = icon("database"), DT::dataTableOutput('contents')),
               tabPanel("Grafik",icon = icon("line-chart"),
                        box(
                            title = "Histogram", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput("wykres", height = 250)
                        )),
               tabPanel("Summary",verbatimTextOutput("summary"))
    )
    ,
    div(id="back",tags$b("Development by Şükrü ERGÜNTOP")),
    tags$style(type="text/css", "#back {position:absolute;bottom:0;color:#088da5;max-width: 100%; width: 100%;}")
)