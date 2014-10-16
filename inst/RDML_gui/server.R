library(shiny)
library(RDML)
library(chipPCR)

shinyServer(function(input, output, session) {
  
  ########## UI elements
  
  output$ui.targets <- renderUI({
    if(is.null(input$rdml.file) ||
         is.null(input$dataTypeSelector))
      return()
    vals$targets <- rdml.obj()$targets
    selectInput("targets", "Targets:",
                vals$targets,
                multiple = TRUE)    
  })
  
  output$ui.types <- renderUI({
    if(is.null(rdml.obj()) ||
         is.null(input$dataTypeSelector))
      return()
    if(is.null(input$targets)) targets <- vals$targets
    else targets <- input$targets    
    vals$types <- unique(as.vector(sapply(targets, function(fluorTarget) {
      rdml.obj()$types[[fluorTarget]]})))    
    selectInput("types", "Types:",
                vals$types,
                multiple = TRUE)
  })
  
  output$ui.dtypeSelector <- renderUI({
    if(is.null(rdml.obj()))
      return()
    dtypeSelectorVars <- c()
    vals$melt <- TRUE
    if("qPCR" %in% rdml.obj()$used.methods) 
    {
      dtypeSelectorVars <- c(dtypeSelectorVars, 
                             "qPCR" = "qPCR")
      vals$melt <- FALSE
    }
    if("melt" %in% rdml.obj()$used.methods) 
      dtypeSelectorVars <- c(dtypeSelectorVars, 
                             "Melting" = "melt")
    
    radioButtons("dataTypeSelector",
                 "Show Data:",
                 dtypeSelectorVars
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
                     vals$rdml.obj <- RDML$new(input$rdml.file$datapath,
                                               name.pattern = input$pattern)                     
                   })
      
      
    })    
    return(vals$rdml.obj)
  })
  
# })

#   vals$dtypeSelectorVars <- #c("qPCR" = "qPCR", "Melting" = "melt")
#     reactive({
#     if(is.null(rdml.obj))      
#       return(NULL)
#     isolate({
#     dtypeSelectorVars <- c()
#     vals$melt <- TRUE
#     if("qPCR" %in% rdml.obj()$used.methods) 
#     {
#       dtypeSelectorVars <- c(dtypeSelectorVars, 
#                                   "qPCR" = "qPCR")
#       vals$melt <- FALSE
#     }
#     if("melt" %in% rdml.obj()$used.methods) 
#       dtypeSelectorVars <- c(dtypeSelectorVars, 
#                                   "Melting" = "melt")
#     })
#     print("ok")
#     dtypeSelectorVars
#     })
#   

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
  
  ## color input not works for now!!4^!
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
{ rdml.obj()$Summarize()
})
})

fdata.data <- reactive({
  filt <- list(method = input$dataTypeSelector)
  if(!is.null(input$targets))
    filt$targets <- input$targets
  if(!is.null(input$types))
    filt$types <- input$types
  filt$names <- input$names.filter
  
  vals$fdata <- rdml.obj()$GetFData(filter = filt)
  vals$fdata
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
    lab <- ifelse(input$dataTypeSelector == "qPCR",
                  "Cycles",
                  "Temperature")
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
    write.csv(rdml.obj()$GetFData(filter = list(
      method = input$dataTypeSelector)),
      file)
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