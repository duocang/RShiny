3
library(shinydashboard)
library(shinyBS)
library(DT)
library(leaflet)
source("user.R")
source("admin.R")
my_username <- c("test","admin")
my_password <- c("test","123")
get_role=function(user){
    
    if(user=="test") {
        
        return("TEST")
    }else{
        
        return("ADMIN")
    }
}

get_ui=function(role){
    itog=list()
    if(role=="TEST"){
        itog$title=test_title
        itog$system=test_system
        itog$main=test_main
        itog$side=test_side
        return(itog)
    }else{
        itog$title=admin_title
        itog$system=admin_system
        itog$main=admin_main
        itog$side=admin_side
        return(itog)
    }
}


shinyServer(function(input, output,session) {
    
    
    
    USER <- reactiveValues(Logged = FALSE,role=NULL)
    
    
    
    
    data <- reactive({
        file2 <- input$file1
        if(is.null(file2)){return()}
        read.csv(file=file2$datapath,header=TRUE,sep=input$sep,stringsAsFactors =FALSE)
        
    })
    
    output$wykres <- renderPlot({
        x    <- data()[, input$xcol] 
        y    <- data()[, input$ycol] 
        z    <- data()[, input$zcol] 
        req(as.numeric(x))
        req(as.numeric(y))
        plot(x,y,data=data())
    })
    
    
    
    output$summary <- renderPrint({
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        summary( read.csv(inFile$datapath, header=TRUE,sep=input$sep))
    })
    
    
    observe({
        inFile<-input$file1
        print(inFile)
        if(is.null(inFile))
            return(NULL)
        dt = read.csv(inFile$datapath, sep=input$sep)
        nums <- sapply(dt, is.numeric)
        items=names(nums[nums])
        names(items)=items
        ## Decide later what to do with the data, here we just fill
        updateSelectInput(session, "xcol", choices = items)
        updateSelectInput(session, "ycol", choices = items,selected = names(dt)[2])
        updateSelectInput(session, "zcol", choices = names(dt))
    })
    
    
    output$contents <-  DT::renderDataTable({
        if(is.null(data())){
            return(NULL) 
        }
        
        DT::datatable(data(),extensions = 'FixedColumns',class = 'cell-border stripe table-bordered',style = 'bootstrap',
                      filter = list(position = "top", clear = TRUE),
                      options = list(
                          scrollX = TRUE,
                          autoWidth = TRUE, 
                          #fixedColumns = list(leftColumns = 2, rightColumns = 1),
                          columnDefs = list(list(width = "140px", targets = "_all"))
                      ),
                      rownames = FALSE,
                      selection = "none")
    })
    
    
    
    observe({ 
        if (USER$Logged == FALSE) {
            if (!is.null(input$Login)) {
                if (input$Login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    Id.username <- which(my_username == Username)
                    Id.password <- which(my_password == Password)
                    if (length(Id.username) > 0 & length(Id.password) > 0) {
                        if (Id.username == Id.password) {
                            USER$Logged <- TRUE
                            USER$role=get_role(Username)
                            
                        }
                    } 
                }
            }
        }
    })
    
    
    
    
    observe({
        if (USER$Logged == FALSE) {
            
            output$title <- renderText({
                "R Web Application"
            })
            
            output$side <- renderUI({
                div(id = "login",
                    wellPanel(textInput("userName", tags$span(style="color:RGB(54,127,169)", "Username:")),
                              tags$style(type="text/css", "#userName {text-align:center;}"),
                              passwordInput("passwd", tags$span(style="color:RGB(54,127,169)", "Password:")),
                              tags$style(type="text/css", "#passwd {text-align:center;}"),
                              br(),
                              fluidRow(
                                  column(4,
                                         actionButton("Login", tags$span(style="color:RGB(54,127,169)", "Login:"))
                                  ),
                                  column(8,
                                         div(img(src="r.png",width=40, heigth=3),align = "right")
                                  )
                              ))
                )
                
            })
            
            
            output$page <- renderUI({
                div(id="log1",img(src="data.png",width=700),align = "center")
            })
        }
        if (USER$Logged == TRUE)    {
            itog=get_ui(USER$role)
            output$title<- renderText({
                itog$title
            })
            output$system<- renderUI({
                itog$system
            })
            output$side <- renderUI({
                itog$side
            })
            output$page <- renderUI({
                itog$main
            })
        }
    })  
})