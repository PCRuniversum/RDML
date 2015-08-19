
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RDML)
library(plyr)
library(dplyr)

testEmptyInput <- function(val) {
  if(is.null(val) || is.na(val) || val == "")
    return(NULL)
  val
}

testNull <- function(val) {
  if(is.null(val))
    return(NA)
  val
}

genErrorMsg <- function(rowName, message) {
  sprintf("<p>Row: %s<br>%s</p>",
          rowName, 
          message)
}


tblHeight <- 500

shinyServer(function(input, output, session) {
  values <- reactiveValues(RDMLs = list())
  
  # Files Tab ------------------------------------------------------------
  
  # load files
  observe({
    if (is.null(input$rdmlFiles))
      return(NULL)
    isolate({
      for(i in 1:length(input$rdmlFiles$name)) {
        values$RDMLs[[input$rdmlFiles$name[i]]] <-
          RDML$new(input$rdmlFiles$datapath[i])
      }
    })
  })
  
  # update rdmlFileSlct values
  observe({
    if (is.null(values$RDMLs))
      return(NULL)
    isolate({
      updateSelectizeInput(session,
                           "rdmlFileSlct",
                           choices = names(values$RDMLs),
                           selected = input$rdmlFileSlct)
    })
  })
  
  # on rdmlFileSlct change
  observe({
    if (input$rdmlFileSlct == "") {
      return(NULL)
    }
    isolate({
      # create new empty file
      if (!(input$rdmlFileSlct %in% names(values$RDMLs))) {
        values$RDMLs[[input$rdmlFileSlct]] <- RDML$new()
      }
      # update mergeRdmlsSlct values
      updateSelectInput(session,
                        "mergeRdmlsSlct",
                        choices = names(values$RDMLs)[names(values$RDMLs) != input$rdmlFileSlct],
                        selected = NULL)
      # clone selected too temp RDML 
      values$rdml <- values$RDMLs[[input$rdmlFileSlct]]$clone(deep = TRUE)
      # update fields
      updateTextInput(session,
                      "dateMadeText",
                      value = testNull(values$rdml$dateMade))
      updateTextInput(session,
                      "dateUpdatedText",
                      value = testNull(values$rdml$dateUpdated))
      
      # update selectors
      # values$updateSlct <- TRUE
      #       updateSelectizeInput(session,
      #                            "idSlct",
      #                            choices = names(values$rdml))
    })
  })
  
  # write to RDML
  observe({
    if (is.null(values$rdml))
      return(NULL)
    values$rdml$dateMade <- testEmptyInput(input$dateMadeText)
    values$rdml$dateUpdated <- testEmptyInput(input$dateUpdatedText)
  })
  
  # remove RDML
  observe({
    input$removeRDMLBtn
    isolate({
      values$rdml <- NULL
      values$RDMLs[[input$rdmlFileSlct]] <- NULL
    })
  })
  
  # update RDML
  observe({
    input$updateRDMLBtn
    isolate({
      if (is.null(values$rdml))
        return(NULL)
      values$RDMLs[[input$rdmlFileSlct]] <- values$rdml$clone(deep = TRUE)
    })
  })
  
  # ID Tab ----------------------------------------------------------------  
  
  # init
  observe({
    if (is.null(values$rdml$id))
      return(NULL)
    isolate({
      updateSelectizeInput(session,
                           "idSlct",
                           choices = names(values$rdml$id))
    })
  })
  
  # on idSlct change
  observe({
    if (input$idSlct == "") {
      return(NULL)
    }
    isolate({
      # update fields
      if (!is.null(values$rdml$id[[input$idSlct]])) {
        id <- values$rdml$id[[input$idSlct]]
        updateTextInput(session,
                        "idPublisherText",
                        value = testNull(id$publisher))
        updateTextInput(session,
                        "idSerialNumberText",
                        value = testNull(id$serialNumber))
        updateTextInput(session,
                        "idMD5HashText",
                        value = testNull(id$MD5Hash))
      } else {
        updateTextInput(session,
                        "idPublisherText",
                        value = input$idSlct)
      }
    })
  })
  
  # write to ID
  observe({
    if (is.null(testEmptyInput(input$idPublisherText))) {
      return(NULL)
    }
    tryCatch({
      id <- rdmlIdType$new(
        publisher = testEmptyInput(input$idPublisherText),
        serialNumber = testEmptyInput(input$idSerialNumberText),
        MD5Hash = testEmptyInput(input$idMD5HashText))
      isolate({
        values$rdml$id[[input$idSlct]] <- id
        # rename list elements
        if (input$idSlct != input$idPublisherText) {
          values$rdml$id <- values$rdml$id
          updateSelectizeInput(session,
                               "idSlct",
                               choices = names(values$rdml$id),
                               selected = input$idPublisherText)
        }
      })
    },
    error = function(e) print(e$message)
    )
  })
  
  # remove ID
  observe({
    input$removeIDBtn
    isolate({
      values$rdml$id[[input$idSlct]] <- NULL
      updateSelectizeInput(session,
                           "idSlct",
                           choices = names(values$rdml$id))
    })
  })
  
  # Experimenter Tab ----------------------------------------------------------------  
  
  # init
  observe({
    if (is.null(values$rdml$experimenter))
      return(NULL)
    isolate({
      updateSelectizeInput(session,
                           "experimenterSlct",
                           choices = names(values$rdml$experimenter))
    })
  })
  
  # on experimenterSlct change
  observe({
    if (input$experimenterSlct == "") {
      return(NULL)
    }
    isolate({
      # update fields
      if (!is.null(values$rdml$experimenter[[input$experimenterSlct]])) {
        experimenter <- values$rdml$experimenter[[input$experimenterSlct]]
        updateTextInput(session,
                        "experimenterIdText",
                        value = testNull(experimenter$id))
        updateTextInput(session,
                        "experimenterFirstNameText",
                        value = testNull(experimenter$firstName))
        updateTextInput(session,
                        "experimenterLastNameText",
                        value = testNull(experimenter$lastName))
        updateTextInput(session,
                        "experimenterEmailText",
                        value = testNull(experimenter$email))
        updateTextInput(session,
                        "experimenterLabNameText",
                        value = testNull(experimenter$labName))
        updateTextInput(session,
                        "experimenterLabAddressText",
                        value = testNull(experimenter$labAddress))
      } else {
        updateTextInput(session,
                        "publisherText",
                        value = input$experimenterSlct)
      }
    })
  })
  
  # write to experimenter
  observe({
    if (is.null(testEmptyInput(input$publisherText))) {
      return(NULL)
    }
    tryCatch({
      isolate({
        experimenter <- values$rdml$experimenter[[input$experimenterSlct]]
      })
      experimenter <- experimenterType$new(
        testEmptyInput(input$experimenterIdText),
        testEmptyInput(input$experimenterFirstNameText),
        testEmptyInput(input$experimenterLastNameText),
        testEmptyInput(input$experimenterEmailText),
        testEmptyInput(input$experimenterLabNameText),
        testEmptyInput(input$experimenterLabAddressText))
      isolate({
        values$rdml$experimenter[[input$experimenterSlct]] <- experimenter
        # rename list elements
        if (input$experimenterSlct != input$experimenterIdText) {
          values$rdml$experimenter <- values$rdml$experimenter
          updateSelectizeInput(session,
                               "experimenterSlct",
                               choices = names(values$rdml$experimenter),
                               selected = input$experimenterIdText)
        }
      })
    },
    error = function(e) print(e$message)
    )
  })
  
  # remove experimenter
  observe({
    input$removeExperimenterBtn
    isolate({
      values$rdml$experimenter[[input$experimenterSlct]] <- NULL
      updateSelectizeInput(session,
                           "experimenterSlct",
                           choices = names(values$rdml$experimenter))
    })
  })
  
  
  
  # Documentation Table -----------------------------------------------------  
  
  output$documentationTbl <- renderRHandsontable({
    df <- data.frame(
      id = c("", ""),
      text = c("", ""),
      stringsAsFactors = FALSE)
    if (!is.null(values$rdml)) {
      for(el in values$rdml$documentation) {
        df <- 
          rbind(df,
                c(el$id$id,
                  el$text))
      }
    }
    rhandsontable(df, rowHeaders = NULL,
                  height = tblHeight) %>% 
      hot_table(allowColEdit = FALSE)
  })
  
  
  observe({
    if (is.null(input$documentationTbl)) {
      return(NULL)
    }
    isolate({
      if (is.null(values$rdml)) {
        return(NULL)
      }
      df <- hot_to_r(input$documentationTbl)
      values$rdml$documentation <- { 
        l <- apply(df, 1,
                   function(row)
                   {
                     tryCatch({
                       if (!all(is.na(row)) && !all(row == "")) {
                         return(
                           documentationType$new(
                             id = idType$new(testEmptyInput(row["id"])),
                             text = testEmptyInput(row["text"]))
                         )}
                       NULL
                     },
                     error = function(e) {
                       values$log <- c(values$log,
                                       genErrorMsg(row["id"], e$message))
                       NULL
                     })
                   }) %>% compact
        if (is.null(l))
          list()
        else l
      }
    })
  })
  
  # Dye Table -----------------------------------------------------  
  
  output$dyeTbl <- renderRHandsontable({
    df <- data.frame(
      id = c("", ""),
      description = c("", ""),
      stringsAsFactors = FALSE)
    if (!is.null(values$rdml)) {
      for(el in values$rdml$dye) {
        df <- 
          rbind(df,
                c(el$id$id,
                  el$description))
      }
    }
    rhandsontable(df, rowHeaders = NULL,
                  height = tblHeight) %>% 
      hot_table(allowColEdit = FALSE)
  })
  
  
  observe({
    if (is.null(input$dyeTbl)) {
      return(NULL)
    }
    isolate({
      if (is.null(values$rdml)) {
        return(NULL)
      }
      df <- hot_to_r(input$dyeTbl)
      values$rdml$dye <- { 
        l <- apply(df, 1,
                   function(row)
                   {
                     tryCatch({
                       if (!all(is.na(row)) && !all(row == "")) {
                         return(
                           dyeType$new(
                             id = idType$new(testEmptyInput(row["id"])),
                             description = testEmptyInput(row["description"]))
                         )}
                       NULL
                     },
                     error = function(e) {
                       values$log <- c(values$log,
                                       genErrorMsg(row["id"], e$message))
                       NULL
                     })
                   }) %>% compact
        if (is.null(l))
          list()
        else l
      }
    })
  })
  
  # Sample Table -----------------------------------------------------  
  
  output$sampleTbl <- renderRHandsontable({
    df <- data.frame(
      id = c("", ""),
      description = c("", ""),
      type = c("", ""),
      interRunCalibrator = c(FALSE, FALSE),
      q.value = c("", ""),
      q.unit = c("", ""),
      calibratorSample = c(FALSE, FALSE),
      cSM.enzyme = c("", ""),
      cSM.primingMethod = c("", ""),
      cSM.dnaseTreatment = c("", ""),
      cSM.tcc = c("", ""),
      tQ.conc = c("", ""),
      tQ.nucleotide = c("", ""),
      stringsAsFactors = FALSE)
    if (!is.null(values$rdml)) {
      for(el in values$rdml$sample) {
        df <- 
          rbind(df,
                c(
                  el$id$id,
                  el$description,
                  el$type$value,
                  el$interRunCalibrator,
                  el$quantity$value,
                  el$quantity$unit$value,
                  el$calibratorSample,
                  el$cdnaSynthesisMethod$enzyme,
                  el$cdnaSynthesisMethod$primingMethod$value,
                  el$cdnaSynthesisMethod$dnaseTreatment,
                  testNull(el$cdnaSynthesisMethod$
                             thermalCyclingConditions$id),
                  el$templateQuantity$conc,
                  el$templateQuantity$nucleotide$value))
        
      }
    }
    rhandsontable(df, rowHeaders = NULL,
                  height = tblHeight) %>% 
      hot_table(allowColEdit = FALSE,
                highlightRow = TRUE,
                groups = list(list(cols = c(5, 6)),
                              list(cols = 8:11),
                              list(cols = c(12, 13))))
  })
  
  # TODO
  observe({
    if (is.null(input$sampleTbl)) {
      return(NULL)
    }
    isolate({
      if (is.null(values$rdml)) {
        return(NULL)
      }
      df <- hot_to_r(input$sampleTbl)
      values$rdml$dye <- { 
        l <- apply(df, 1,
                   function(row)
                   {
                     tryCatch({
                       if (!all(is.na(row)) && !all(row == "")) {
                         return(
                           sampleType$new(
                             id = idType$new(testEmptyInput(row["id"])),
                             description = testEmptyInput(row["description"]))
                         )}
                       NULL
                     },
                     error = function(e) {
                       values$log <- c(values$log,
                                       genErrorMsg(row["id"], e$message))
                       NULL
                     })
                   }) %>% compact
        if (is.null(l))
          list()
        else l
      }
    })
  })
  
  # Download ----------------------------------------------------------------
  
  output$downloadRDML <- downloadHandler(
    filename = function() {
      paste0(values$selFile, '.RDML')
    },
    content = function(file) {
      # Do not work!!!! ?????
      values$rdml$AsXML(file)
    }
  )
  
  # Log ---------------------------------------------------------------------
  
  
  output$logText <- renderUI({
    if (is.null(values$log))
      return(NULL)
    HTML(values$log)
  })
  
})






