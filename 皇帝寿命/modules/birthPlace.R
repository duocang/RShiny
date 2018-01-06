library(leaflet)
library(dplyr)
library(RColorBrewer)
dynastyList <- unique(personalInfo$朝代)
emperorListAll <- personalInfo$皇帝
emperorListInDynasty <- personalInfo %>% 
    filter(`朝代`=="秦汉") %>%
    select(`皇帝`) %>%
    as.list()


emperorListInDynasty <- personalInfo %>% 
        filter(`朝代`=="秦汉") %>%
        select(`皇帝`) %>%
        as.list()


################################################################################
#
# UIab
#
################################################################################
birthPlaceInput <- function(
    id,
    title = "皇帝出生地"
){
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    # Wrap all UI content in tagList
    tagList(
        leafletOutput(ns("map"), width = "100%", height = "100%"),
        absolutePanel( fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, height = "auto",
                      h2("统领们"),
                      selectInput(ns("dynasty"), "朝代", dynastyList),
                      uiOutput(ns("ui"))
                      )
    )
}

################################################################################
###
### Server
###
################################################################################
birthPlace <- function(
    input,
    output,
    session,
    title = function() return("皇帝出生地")
){
    ## Interactive Map ###########################################
    # Create the map
    # Create the map
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
            setView(lng = 120, lat = 37.45, zoom = 4)
    })
    
    output$ui <- renderUI({
        ns <- session$ns
        
        selectInput(ns("emperorInDynasty"), "皇帝", "")
        
    })
    
    # the list of emperor in selection
    emperorLisIndynasty <- reactive({
        emperorListInDynasty <- personalInfo %>% 
            filter(`朝代`==input$dynasty) %>%
            select(`皇帝`)
    })
    # 更行皇帝选项的内容
    observe({
        updateSelectInput(session, "emperorInDynasty", choices = emperorLisIndynasty())
    })

    

    
}




