library(shiny)

shinyServer(function(input, output) {
  rdml.obj <- reactive({
    if(is.null(input$rdml.file))
      return(NULL)
    RDML(input$rdml.file$datapath,
                name.pattern = input$pattern,
                flat.table = input$flat.table,
                omit.ntp = input$omit.ntp)
    }
  )
  
  output$rdml.summary.out <- renderPrint({        
    if (is.null(rdml.obj()))
      return("Upload your data first!")    
    str(rdml.obj())
  })
})