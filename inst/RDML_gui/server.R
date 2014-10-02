library(shiny)
library(RDML)
library(chipPCR)

shinyServer(function(input, output, session) {
  
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
    isolate({
      withProgress(message = "Processing RDML data",
                   value = 0, {
                     vals$rdml.obj <- RDML(input$rdml.file$datapath,
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
                   })
      return(vals$rdml.obj)
    })
  })
  
  output$overview.plot <- renderPlot({
    if(is.null(rdml.obj()))
      return()
    print("updating plot")
    sep.col <- c("")
    if(input$sep.name.col) sep.col <- c(sep.col, "name")
    if(input$sep.type.col) sep.col <- c(sep.col, "type")
    if(input$sep.targets.col) sep.col <- c(sep.col, "targets")
    
    sep.note <- c("")
    if(input$sep.name.note) sep.note <- c(sep.note, "name")
    if(input$sep.type.note) sep.note <- c(sep.note, "type")
    if(input$sep.targets.note) sep.note <- c(sep.note, "targets")
    
    ## color input not works for now!!!
    col <- gsub("[[:blank:]]","", input$colors.overview)    
    if(col != "") col <- unlist(strsplit(col, "[,]"))    
    
    vals$pl <- plot(rdml.obj(),
                    print.legend = FALSE,
                    separate.by = list(left = sep.col,
                                       right = sep.note)
    )
    #     isolate ({
    #     updateTextInput(session,
    #                     "colors.overview",
    #                     "Use Colors:",
    #                     paste(levels(vals$pl$Legend$Color), collapse = ", ")
    #                     )
    #     })
    #     return(pl)
  })
  
  output$overview.legend <- renderTable({
    if (is.null(vals$pl))
      return()
    vals$pl
  })
  
#   observe({
#     if(input$rand.col.overview > 0){
#       isolate ({        
#         updateTextInput(session, "colors.overview", "Use Colors:", " ")
#       })
#     }
#   })
  
  output$rdml.summary.out <- renderPrint({        
    if (is.null(rdml.obj()))
      return()    
    withProgress(message = "Generating summary",
                 value = 0, 
                 { summary(rdml.obj())
                  })
  })
  
  fdata.data <- reactive({
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
  output$fdata.table <- renderTable({
    if(is.null(rdml.obj()))
      return()
    fdata.data()
  })  
  
  output$fdata.plot <- renderPlot({
    if(is.null(rdml.obj()))
      return()
    if(input$plotStyle == "single") {
      lab <- ifelse(vals$melt,
                    "Temperature",
                    "Cycles")
      matplot(fdata.data()[1], 
              fdata.data()[-1],
              xlab = lab,
              ylab = "Fluorescence",
              type = "l")
    }
    else plotCurves(fdata.data()[, 1], fdata.data()[, -1], type = "l")   
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