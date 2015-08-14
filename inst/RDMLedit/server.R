
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RDML)
library(plyr)
library(dplyr)

testValue <- function(val) {
  if(is.null(val))
    return(NA)
  val
}


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
  
  output$downloadRDML <- downloadHandler(
    filename = function() {
      paste0(values$selFile, '.RDML')
    },
    content = function(file) {
      values$rdml$AsXML(file)
    }
  )
  
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
                c(testValue(id$publisher),
                  testValue(id$serialNumber),
                  testValue(id$MD5Hash)))
      }
    }
    rhandsontable(df, rowHeaders = NULL) %>% 
      hot_table(allowColEdit = FALSE)
  })
  
  observe({
    if (is.null(input$idTbl)) {
      return(NULL)
    }
    isolate({
      df <- hot_to_r(input$idTbl)
      print("s")
      apply(df, 1,
            function(row)
            {
              tryCatch({
                
                if (!is.null(values$rdml$id[[row["id"]]])) {
                  
                  values$rdml$id[[row["id"]]] <-
                    rdmlIdType$new(
                      publisher = row["publisher"],
                      serialNumber = row["serialNumber"],
                      MD5Hash = row["MD5Hash"]
                    )
                } else {
                  if (!is.na(row["id"]) && row["id"] != "") {
                  values$rdml$id <- c(
                    values$rdml$id,
                    rdmlIdTypea$new(
                      publisher = row["publisher"],
                      serialNumber = row["serialNumber"],
                      MD5Hash = row["MD5Hash"]
                    ))
                  }
                }
                
              },
              error = function(e) {
                values$error <- e
                print(values$error)
              })
            })
    })
  })
  
  output$errorText <- renderText({
    if(is.null(values$error))
      return(NULL)
    values$error
  })
  
  # Experimenter Table ----------------------------------------------------------------  
  output$experimenterTbl <- renderRHandsontable({
    df <- data.frame(
      id = c("", ""),
      firstName = c("", ""),
      lastName = c("", ""),
      email = c("", ""),
      labName = c("", ""),
      labAdress = c("", ""),
      stringsAsFactors = FALSE)
    if (!is.null(values$rdml)) {
      for(el in values$rdml$experimenter) {
        df <- 
          rbind(df,
                c(testValue(el$id$id),
                  testValue(el$firstName),
                  testValue(el$lastName),
                  testValue(el$email),
                  testValue(el$labName),
                  testValue(el$labAdress)))
      }
    }
    rhandsontable(df, rowHeaders = NULL) %>% 
      hot_table(allowColEdit = FALSE)
  })
})