library(shiny)
library(RDML)
library(chipPCR)

shinyServer(function(input, output) {
  
  ########## UI elements
  
  output$ui.targets <- renderUI({
    if(is.null(input$rdml.file) ||
         is.null(input$dataTypeSelector))
      return()
    vals$targets <- names(rdml.obj()[[input$dataTypeSelector]])
    selectInput("targets", "Targets:",
                vals$targets,
                multiple = TRUE)
  })
  
  output$ui.types <- renderUI({
    if(is.null(rdml.obj()) ||
         is.null(input$dataTypeSelector))
      return()
    targets <- ifelse(is.null(input$targets),
                      vals$targets,
                      input$targets)
    vals$types <- unique(as.vector(sapply(targets, function(fluorTarget) {
      names(rdml.obj()[[input$dataTypeSelector]][[fluorTarget]])})))
    selectInput("types", "Types:",
                vals$types,
                multiple = TRUE)
  })
  
  output$ui.dtypeSelector <- renderUI({
    if(is.null(rdml.obj()))
      return()
    radioButtons("dataTypeSelector",
                 "Show Data:",
                 vals$dtypeSelectorVars
    )
  })
  
  ######################
  
  vals <- reactiveValues()
   
  rdml.obj <- reactive({
    if(is.null(input$rdml.file))      
      return(NULL)
    input$update
    isolate({ vals$rdml.obj <- RDML(input$rdml.file$datapath,
                                    name.pattern = input$pattern,
                                    omit.ntp = input$omit.ntp)
              vals$dtypeSelectorVars <- c()
              vals$melt <- TRUE
              if("qPCR" %in% names(vals$rdml.obj)) 
              {
                vals$dtypeSelectorVars <- c(vals$dtypeSelectorVars, 
                                            "qPCR" = "qPCR")
                vals$melt <- FALSE
              }
              if("Melt" %in% names(vals$rdml.obj)) 
                vals$dtypeSelectorVars <- c(vals$dtypeSelectorVars, 
                                            "Melting" = "Melt")
              
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
  
  overview.data <- reactive({
    targets <- ifelse(is.null(input$targets),
                      NA,
                      input$targets)
    types <- ifelse(is.null(input$types),
                    NA,
                    input$types)
    names.filter <- ifelse(input$names.filter == '',
                           NA,
                           input$names.filter)
    if(!is.null(input$dataTypeSelector)) {
      vals$melt <- ifelse((input$dataTypeSelector == 'Melt'),
                          TRUE,
                          FALSE)
    }
    selectFData(rdml.obj(),
                melt = vals$melt,
                targets = targets,
                types = types,
                snames = names.filter)
  })
  
  
  
  # TODO: split tables by X columns
  output$overview.table <- renderTable({
    if(is.null(rdml.obj()))
      return()
    overview.data()
  })  
  
output$overview.plot <- renderPlot({
    if(is.null(rdml.obj()))
      return()
    if(input$plotStyle == "single") {
      lab <- ifelse(vals$melt,
                    "Temperature",
                    "Cycles")
      matplot(overview.data()[1], 
              overview.data()[-1],
              xlab = lab,
              ylab = "Fluorescence",
              type = "l")
    }
    else plotCurves(overview.data()[, 1], overview.data()[, -1], type = "l")   
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
      write.csv(selectFData(rdml.obj(), melt = vals$melt()), file)
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
  
  ######################
})