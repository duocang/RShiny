library(shiny)
library(shinydashboard)
library(leaflet)
shinyUI( 
    dashboardPage(
        dashboardHeader( title=textOutput("title"),titleWidth = 300,
                         tags$li(class = "dropdown",
                                 tags$a(href="https://www.r-project.org", target="_blank", 
                                        tags$img(height = "20px", alt="SBM Logo", src="r.png")
                                 ))
                         
        ),
        dashboardSidebar(uiOutput("side"),uiOutput('Side_Logout'), width = 300),
        dashboardBody(
            tags$head(tags$style(HTML('
                                      /* logo */
                                      .skin-blue .main-header .logo {
                                      background-color: #088da5;
                                      }
                                      
                                      /* logo when hovered */
                                      .skin-blue .main-header .logo:hover {
                                      background-color: #9cd1db;
                                      }
                                      
                                      /* navbar (rest of the header) */
                                      .skin-blue .main-header .navbar {
                                      background-color: #39a3b7
                                      ;
                                      }        
                                      
                                      /* main sidebar */
                                      .skin-blue .main-sidebar {
                                      background-color:  #39a3b7
                                      }
                                      
                                      /* active selected tab in the sidebarmenu */
                                      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                      background-color: RGB(8,141,168);
                                      color: #ffffff;
                                      
                                      }
                                      
                                      /* other links in the sidebarmenu */
                                      .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                      background-color: #f9f9f9;
                                      color: #000000;
                                      border: 0.9px solid #20b2aa;
                                      }
                                      
                                      /* other links in the sidebarmenu when hovered */
                                      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                      background-color: RGB(8,141,168);
                                      color: #ffffff;
                                      }
                                      /* toggle button when hovered  */                    
                                      .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                      background-color: #9cd1db;
                                      color: #ffffff;
                                      }
                                      '))),
            uiOutput("page")
            )
            )
    
            )
