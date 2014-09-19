library(shiny)
library(RDML)
library(chipPCR)

shinyServer(function(input, output) {
  vals <- reactiveValues()
  
  rdml.obj <- reactive({
    if(is.null(input$rdml.file))      
      return(NULL)
    input$update
    isolate({ vals$rdml.obj = RDML(input$rdml.file$datapath,
                   name.pattern = input$pattern,
                   flat.table = input$flat.table,
                   omit.ntp = input$omit.ntp)
              return(vals$rdml.obj)
    })
  })  
  
  output$rdml.structure.out <- renderPrint({        
    if (is.null(rdml.obj()))
      return()    
    str(rdml.obj())
  })
  
  output$rdml.summary.out <- renderPrint({        
    if (is.null(rdml.obj()))
      return()    
    summary(rdml.obj())
  })
  output$ui.targets <- renderUI({
    if(is.null(input$rdml.file))
      return()
    vals$targets <- names(rdml.obj()$qPCR)
    selectInput("targets", "Targets:",
                vals$targets,
                multiple = TRUE)
  })
  
  output$ui.types <- renderUI({
    if(is.null(rdml.obj()))
      return()
    targets <- ifelse(is.null(input$targets),
                      vals$targets,
                      input$targets)
    vals$types <- unique(as.vector(sapply(targets, function(fluorTarget) {
      names(rdml.obj()$qPCR[[fluorTarget]])})))
    selectInput("types", "Types:",
                vals$types,
                multiple = TRUE)
  }) 
  
  output$overview.table <- renderTable({
    if(is.null(rdml.obj()))
      return()
    targets <- ifelse(is.null(input$targets),
                      NA,
                      input$targets)
    types <- ifelse(is.null(input$types),
                    NA,
                    input$types)
    names.filter <- ifelse(input$names.filter == '',
                           NA,
                           input$names.filter)
    vals$fdata <- selectFData(rdml.obj(),
                              melt = FALSE,
                              targets = targets,
                              types = types,
                              snames = names.filter)
    return(vals$fdata)
  })  
  
  output$overview.plot <- renderPlot({
    if(is.null(rdml.obj()))
      return()
    plotCurves(vals$fdata[,1], vals$fdata[-1], type = "l")
  })
  
  ############# Downloads
  
  output$downloadRdmlObj <- downloadHandler(    
    filename = function() {
      paste('data-', Sys.Date(), '.Rdata', sep='')
    },
    content = function(con) {
      assign("rdml.obj", vals$rdml.obj)
      save(list="rdml.obj", file=con)
    }
  )
  
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(selectFData(rdml.obj()), file)
    }
  )
  
  output$downloadCsvFiltered <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(vals$fdata, file)
    }
  )
})