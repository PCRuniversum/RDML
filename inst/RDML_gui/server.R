library(shiny)

shinyServer(function(input, output) {  
  output$rdml.summary.out <- renderPrint({    
    inFile <- input$rdml_file    
    if (is.null(inFile))
      return(NULL)
    str(RDML(inFile$datapath))
  })
})