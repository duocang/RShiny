function(input, output, session){

    #
    # Call birthPlace module
    #
    callModule(birthPlace, "birthplace")
    
    output$dataTable <- DT::renderDataTable({
        DT::datatable(personalInfo)
    })
}