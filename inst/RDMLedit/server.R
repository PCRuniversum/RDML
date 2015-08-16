
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RDML)
library(plyr)
library(dplyr)

testInputValue <- function(val) {
  if(is.null(val) || is.na(val) || val == "")
    return(NULL)
  val
}


genErrorMsg <- function(rowName, message) {
  sprintf("<p>Row: %s<br>%s</p>",
          rowName, 
          message)
}


tblHeight <- 500

shinyServer(function(input, output, session) {
  values <- reactiveValues()
  
  # Files Table ------------------------------------------------------------
  
  output$filesTbl <- renderRHandsontable({
    df <- data.frame(
      file = c(NA, NA),
      dateMade = c(NA, NA),
      dateUpdated = c(NA, NA),
      merge = c(FALSE, FALSE),
      stringsAsFactors = FALSE
    )
    if (!is.null(input$rdmlFiles)) {
      isolate({
        for(i in 1:length(input$rdmlFiles$name)) {
          values$RDMLs[[input$rdmlFiles$name[i]]] <-
            RDML$new(input$rdmlFiles$datapath[i])
        }
        for(name in names(values$RDMLs)) {
          df <- 
            rbind(df,
                  c(file = name,
                    dateMade = values$RDMLs[[name]]$dateMade,
                    dateUpdated = values$RDMLs[[name]]$dateUpdated,
                    merge = FALSE,
                    stringsAsFactors = FALSE))
        }
      })
    }
    df$merge <- as.logical(df$merge)
    rhandsontable(df, rowHeaders = NULL,
                  height = tblHeight,
                  selectCallback = TRUE) %>% 
      hot_table(allowColEdit = FALSE,
                highlightRow = TRUE
                #                 ,
                #                 groups = list(list(cols = c(1, 2))
                # )
      )
  })
  
  observe({
    if (is.null(input$filesTbl)) {
      return(NULL)
    }
    df <- hot_to_r(input$filesTbl)
    apply(df, 1, 
          function(row) {
            if (!is.null(values$RDMLs[[row["file"]]])) {
              values$RDMLs[[row["file"]]]$dateMade <- row["dateMade"]
              values$RDMLs[[row["file"]]]$dateUpdated <- row["dateUpdated"]
            } else {
              if (!is.na(row["file"]) && row["file"] != "") {
                values$RDMLs[[row["file"]]] <- RDML$new()
              }
            }
          })
  })
  
  observe({
    if (!is.null(input$filesTbl_select)) {
      row <- input$filesTbl_select$select$r + 1
      values$selFile <- input$filesTbl_select$data[[row]][[1]]
      if (!is.null(values$RDMLs[[values$selFile]])) {
        values$rdml <- values$RDMLs[[values$selFile]]$clone(deep = TRUE)
      } else {
        values$rdml <- NULL
      }
    }
  })
  
  output$selectedFileText <- renderText({
    if (is.null(values$selFile))
      return(NULL)
    sprintf("Selected file: %s", values$selFile)
  })
  
  # ID Table ----------------------------------------------------------------  
  
  output$idTbl <- renderRHandsontable({
    df <- data.frame(
      publisher = c("", ""),
      serialNumber = c("", ""),
      MD5Hash = c("", ""),
      stringsAsFactors = FALSE)
    if (!is.null(values$rdml)) {
      for(id in values$rdml$id) {
        df <- 
          rbind(df,
                c(id$publisher,
                  id$serialNumber,
                  id$MD5Hash))
      }
    }
    rhandsontable(df, rowHeaders = NULL,
                  height = tblHeight) %>% 
      hot_table(allowColEdit = FALSE)
  })
  
  observe({
    if (is.null(input$idTbl)) {
      return(NULL)
    }
    isolate({
      if (is.null(values$rdml)) {
        return(NULL)
      }
      df <- hot_to_r(input$idTbl)
      values$rdml$id <- { 
        l <- apply(df, 1,
               function(row)
               {
                 tryCatch({
                   if (!all(is.na(row)) && !all(row == "")) {
                     return(
                       rdmlIdType$new(
                         publisher = testInputValue(row["publisher"]),
                         serialNumber = testInputValue(row["serialNumber"]),
                         MD5Hash = testInputValue(row["MD5Hash"]))
                     )}
                   NULL
                   },
                   error = function(e) {
                     values$log <- c(values$log,
                                     genErrorMsg(row["publisher"], e$message))

                     NULL
                   })
                 }) %>% compact
        if (is.null(l))
          list()
        else l
      }
    })
  })
  

  output$logText <- renderUI({
    if (is.null(values$log))
      return(NULL)
    HTML(values$log)
  })
  
  # Experimenter Table ----------------------------------------------------------------  
  output$experimenterTbl <- renderRHandsontable({
    df <- data.frame(
      id = c("", ""),
      firstName = c("", ""),
      lastName = c("", ""),
      email = c("", ""),
      labName = c("", ""),
      labAddress = c("", ""),
      stringsAsFactors = FALSE)
    if (!is.null(values$rdml)) {
      for(el in values$rdml$experimenter) {
        df <- 
          rbind(df,
                c(el$id$id,
                  el$firstName,
                  el$lastName,
                  el$email,
                  el$labName,
                  el$labAddress))
      }
    }
    rhandsontable(df, rowHeaders = NULL,
                  height = tblHeight) %>% 
      hot_table(allowColEdit = FALSE)
  })
  
  
  observe({
    if (is.null(input$experimenterTbl)) {
      return(NULL)
    }
    isolate({
      if (is.null(values$rdml)) {
        return(NULL)
      }
      df <- hot_to_r(input$experimenterTbl)
      values$rdml$experimenter <- { 
        l <- apply(df, 1,
                   function(row)
                   {
                     tryCatch({
                       if (!all(is.na(row)) && !all(row == "")) {
                         return(
                           experimenterType$new(
                             id = idType$new(testInputValue(row["id"])),
                             firstName = testInputValue(row["firstName"]),
                             lastName = testInputValue(row["lastName"]),
                             email = testInputValue(row["email"]),
                             labName = testInputValue(row["labName"]),
                             labAddress = testInputValue(row["labAddress"]))
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
                             id = idType$new(testInputValue(row["id"])),
                             text = testInputValue(row["text"]))
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
                             id = idType$new(testInputValue(row["id"])),
                             description = testInputValue(row["description"]))
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






