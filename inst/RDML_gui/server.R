library(shiny)
library(RDML)
library(chipPCR)

shinyServer(function(input, output) {
  rdml.obj <- reactive({
    if(is.null(input$rdml.file))      
      return(NULL)
    input$update
    isolate({ RDML(input$rdml.file$datapath,
                name.pattern = input$pattern,
                flat.table = input$flat.table,
                omit.ntp = input$omit.ntp)
              })
    }
  )
  
  output$rdml.structure.out <- renderPrint({        
    if (is.null(rdml.obj()))
      return("Upload your data first!")    
    str(rdml.obj())
  })
  
  output$rdml.summary.out <- renderPrint({        
    if (is.null(rdml.obj()))
      return("Upload your data first!")    
    summary(rdml.obj())
  })
  output$ui.targets <- renderUI({
    if(is.null(input$rdml.file))
      return()
    selectInput("targets", "Targets:",
                names(rdml.obj()$qPCR),
                multiple = TRUE)
  })
  
  output$ui.types <- renderUI({
    if(is.null(input$rdml.file))
      return()
    targets <- ifelse(is.null(input$targets),
                      names(rdml.obj()$qPCR),
                      input$targets)
    types <- as.vector(sapply(targets, function(fluorTarget) {
      names(rdml.obj()$qPCR[[fluorTarget]])      
    }))    
    types <- unique(types)
    selectInput("types", "Types:",
                types,
                multiple = TRUE)
  })
  
  # rewrite with reactive
  output$overview.table <- renderTable({
    if(is.null(input$rdml.file))
      return()
    targets <- ifelse(is.null(input$targets),
                      NA,
                      input$targets)
    types <- ifelse(is.null(input$types),
                      NA,
                      input$types)
    selectFData(rdml.obj(),
                melt = FALSE,
                targets = targets,
                types = types,
                snames = input$names.filter)
  })
  
  # rewrite with reactive
  output$overview.plot <- renderPlot({
    if(is.null(input$rdml.file))
      return()
    targets <- ifelse(is.null(input$targets),
                      NA,
                      input$targets)
    types <- ifelse(is.null(input$types),
                    NA,
                    input$types)
    fdata <- selectFData(rdml.obj(),
                melt = FALSE,
                targets = targets,
                types = types,
                snames = input$names.filter)    
    plotCurves(fdata[,1], fdata[-1], type = "l")
    })
})