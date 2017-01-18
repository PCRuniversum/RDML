library(shiny)
library(RDML)
library(dplyr)
library(tools)
library(pipeR)
library(chipPCR)
library(MBmca)
library(dpcR)
library(data.table)
library(ggplot2)
library(plotly)
library(rlist)
library(whisker)
library(stringr)

source("rdml.extensions.R")

testEmptyInput <- function(val) {
  if(is.null(val) || is.na(val) || val == "")
    return(NULL)
  val
}

testNull <- function(val) {
  if(is.null(val))
    return(NA)
  unname(val)
}


shinyServer(function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  
  values <- reactiveValues(RDMLs = list(),
                           log = NULL)
  updLog <- function(error.message) {
    isolate({
      error.message <- sprintf("[%s] %s",
                               format(Sys.time(), "%H:%M:%S"),
                               error.message)
      cat(paste0(error.message, "\n"))
      values$log <- c(values$log, error.message)
    })
  }
  
  # Files Tab ------------------------------------------------------------
  
  # load files
  observe({
    if (is.null(input$rdmlFiles))
      return(NULL)
    isolate({
      withProgress(
        for(i in 1:length(input$rdmlFiles$name)) {
          newpath <- paste(input$rdmlFiles$datapath[i],
                           file_ext(input$rdmlFiles$name[i]),
                           sep = ".")
          file.rename(input$rdmlFiles$datapath[i],
                      newpath)
          values$RDMLs[[paste0(file_path_sans_ext(input$rdmlFiles$name[i]),
                               format(Sys.time(), "@%H%M%S"))]] <-
            RDML$new(newpath)
          incProgress(1,
                      message = {
                        if (i != length(input$rdmlFiles$name))
                          sprintf("Processing: %s", input$rdmlFiles$name[i + 1])
                        else NULL })
        },
        min = 0,
        max = length(input$rdmlFiles$name),
        message = sprintf("Processing: %s", input$rdmlFiles$name[1]))
      
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
                           selected = tail(names(values$RDMLs), 1))
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
      # clone selected to temp RDML 
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
  
  
  # merge RDMLs
  observe({
    input$mergeBtn
    isolate({
      if (is.null(input$mergeRdmlsSlct) || input$mergeRdmlsSlct == "")
        return(NULL)
      withProgress(
        values$rdml <- MergeRDMLs(
          c(values$rdml, values$RDMLs[input$mergeRdmlsSlct])
        ),
        message = "Merging RDMLs. Please wait..."
      )
    })
  })
  
  # dendro plot
  output$dendroRDMLplot <- renderPlot({
    if (is.null(values$rdml))
      return(NULL)
    input$updateDendroPlot
    tryCatch(values$rdml$AsDendrogram(),
             error = function(e) {updLog(e$message)}
    )
  })
  
  # write to RDML
  observe({
    if (is.null(values$rdml))
      return(NULL)
    tryCatch({
      values$rdml$dateMade <- testEmptyInput(input$dateMadeText)
      values$rdml$dateUpdated <- testEmptyInput(input$dateUpdatedText)
    },
    error = function(e) updLog(e$message))
  })
  
  # remove RDML 
  observe({
    input$removeRDMLBtn
    isolate({
      values$rdml <- NULL
      values$RDMLs[[input$rdmlFileSlct]] <- NULL
    })
  })
  
  # create subversion RDML
  observe({
    input$createSubversionRDMLBtn
    isolate({
      if (is.null(values$rdml))
        return(NULL)
      values$rdml$dateUpdated <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      new.name <- str_replace(input$rdmlFileSlct,
                              "@.*$",
                              format(Sys.time(), "@%H%M%S"))
      values$RDMLs[[new.name]] <- values$rdml$clone(deep = TRUE)
    })
  })
  
  # ID Tab ----------------------------------------------------------------  
  
  # init
  observe({
    if (is.null(values$rdml$id))
      return(NULL)
    # updLog("Init ID\n")
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
    error = function(e) updLog(e$message)
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
    # updLog("Init Experimenter\n")
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
                        value = testNull(experimenter$id$id))
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
                        "experimenterIdText",
                        value = input$experimenterSlct)
      }
    })
  })
  
  updExperimenterRefs <- function() {
    updateSelectInput(session,
                      "tccExperimenterSlct",
                      choices = names(values$rdml$experimenter))
    updateSelectInput(session,
                      "runExperimenterSlct",
                      choices = names(values$rdml$experimenter))
  }
  
  # write to experimenter
  observe({
    if (is.null(testEmptyInput(input$experimenterIdText))) {
      return(NULL)
    }
    tryCatch({
      experimenter <- experimenterType$new(
        idType$new(testEmptyInput(input$experimenterIdText)),
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
        updExperimenterRefs()
      })
    },
    error = function(e) updLog(e$message)
    )
  })
  
  # remove experimenter
  observe({
    input$removexperimenterBtn
    isolate({
      values$rdml$experimenter[[input$experimenterSlct]] <- NULL
      updateSelectizeInput(session,
                           "experimenterSlct",
                           choices = names(values$rdml$experimenter))
      updExperimenterRefs()
    })
  })
  
  
  
  # Documentation Tab -----------------------------------------------------  
  
  # init
  observe({
    if (is.null(values$rdml$documentation))
      return(NULL)
    # updLog("Init Documentation\n")
    isolate({
      updateSelectizeInput(session,
                           "documentationSlct",
                           choices = names(values$rdml$documentation))
    })
  })
  
  # on documentationSlct change
  observe({
    if (input$documentationSlct == "") {
      return(NULL)
    }
    isolate({
      # update fields
      if (!is.null(values$rdml$documentation[[input$documentationSlct]])) {
        documentation <- values$rdml$documentation[[input$documentationSlct]]
        updateTextInput(session,
                        "documentationIdText",
                        value = testNull(documentation$id$id))
        updateTextInput(session,
                        "documentationTextText",
                        value = testNull(documentation$text))
      } else {
        updateTextInput(session,
                        "documentationIdText",
                        value = input$documentationSlct)
      }
    })
  })
  
  updDocRefs <- function() {
    updateSelectInput(session,
                      "sampleDocumentationSlct",
                      choices = names(values$rdml$documentation))
    updateSelectInput(session,
                      "targetDocumentationSlct",
                      choices = names(values$rdml$documentation))
    updateSelectInput(session,
                      "experimentDocumentationSlct",
                      choices = names(values$rdml$documentation))
    updateSelectInput(session,
                      "runDocumentationSlct",
                      choices = names(values$rdml$documentation))
  }
  
  # write to documentation
  observe({
    if (is.null(testEmptyInput(input$documentationIdText))) {
      return(NULL)
    }
    tryCatch({
      documentation <- 
        documentationType$new(
          idType$new(testEmptyInput(input$documentationIdText)),
          testEmptyInput(input$documentationTextText))
      isolate({
        values$rdml$documentation[[input$documentationSlct]] <- documentation
        # rename list elements
        if (input$documentationSlct != input$documentationIdText) {
          values$rdml$documentation <- values$rdml$documentation
          updateSelectizeInput(session,
                               "documentationSlct",
                               choices = names(values$rdml$documentation),
                               selected = input$documentationIdText)
        }
        updDocRefs()
      })
    },
    error = function(e) updLog(e$message)
    )
  })
  
  # remove documentation
  observe({
    input$removedocumentationBtn
    isolate({
      values$rdml$documentation[[input$documentationSlct]] <- NULL
      updateSelectizeInput(session,
                           "documentationSlct",
                           choices = names(values$rdml$documentation))
      updDocRefs()
    })
  })
  
  # Dye Tab -----------------------------------------------------  
  
  
  # init
  observe({
    if (is.null(values$rdml$dye))
      return(NULL)
    # updLog("Init Dye\n")
    isolate({
      updateSelectizeInput(session,
                           "dyeSlct",
                           choices = names(values$rdml$dye))
    })
  })
  
  
  # on dyeSlct change
  observe({
    if (input$dyeSlct == "") {
      return(NULL)
    }
    isolate({
      # update fields
      if (!is.null(values$rdml$dye[[input$dyeSlct]])) {
        dye <- values$rdml$dye[[input$dyeSlct]]
        updateTextInput(session,
                        "dyeIdText",
                        value = testNull(dye$id$id))
        updateTextInput(session,
                        "dyeDescriptionText",
                        value = testNull(dye$description))
      } else {
        updateTextInput(session,
                        "dyeIdText",
                        value = input$dyeSlct)
      }
    })
  })
  
  updDyeRefs <- function() {
    updateSelectInput(session,
                      "targetDyeIdSlct",
                      choices = names(values$rdml$dye))
  }
  
  # write to dye
  observe({
    if (is.null(testEmptyInput(input$dyeIdText))) {
      return(NULL)
    }
    tryCatch({
      dye <- 
        dyeType$new(
          idType$new(testEmptyInput(input$dyeIdText)),
          testEmptyInput(input$dyeDescriptionText))
      isolate({
        values$rdml$dye[[input$dyeSlct]] <- dye
        # rename list elements
        if (input$dyeSlct != input$dyeIdText) {
          values$rdml$dye <- values$rdml$dye
          updateSelectizeInput(session,
                               "dyeSlct",
                               choices = names(values$rdml$dye),
                               selected = input$dyeIdText)
        }
        updDyeRefs()
        
      })
    },
    error = function(e) updLog(e$message)
    )
  })
  
  
  # remove dye
  observe({
    input$removeDyeBtn
    isolate({
      values$rdml$dye[[input$dyeSlct]] <- NULL
      updateSelectizeInput(session,
                           "dyeSlct",
                           choices = names(values$rdml$dye))
      updDyeRefs()
    })
  })
  
  # Sample Tab -----------------------------------------------------  
  
  
  # init
  observe({
    if (is.null(values$rdml$sample))
      return(NULL)
    # updLog("Init Sample\n")
    isolate({
      updateSelectizeInput(session,
                           "sampleSlct",
                           choices = names(values$rdml$sample))
    })
  })
  
  # on sampleSlct change
  observe({
    if (input$sampleSlct == "") {
      return(NULL)
    }
    # updLog("on sampleSlct change\n")
    isolate({
      if (length(values$rdml$sample[[input$sampleSlct]]$annotation) == 0) {
        updateSelectizeInput(
          session,
          "sampleAnnotationSlct",
          choices = "")
      } else {
        updateSelectizeInput(
          session,
          "sampleAnnotationSlct",
          choices = names(values$rdml$sample[[input$sampleSlct]]$annotation))
      }
      if (length(values$rdml$sample[[input$sampleSlct]]$xRef) == 0) {
        updateSelectizeInput(
          session,
          "samplexRefSlct",
          choices = "")
      } else {
        updateSelectizeInput(
          session,
          "samplexRefnSlct",
          choices = names(values$rdml$sample[[input$sampleSlct]]$annotation))
      }
      # update fields
      if (!is.null(values$rdml$sample[[input$sampleSlct]])) {
        sample <- values$rdml$sample[[input$sampleSlct]]
        updateTextInput(session,
                        "sampleIdText",
                        value = testNull(sample$id$id))
        updateTextInput(session,
                        "sampleDescriptionText",
                        value = testNull(sample$description))
        updateSelectInput(session,
                          "sampleDocumentationSlct",
                          selected = names(sample$documentation))
        updateSelectizeInput(session,
                             "samplexRefSlct",
                             choices = names(sample$xRef))
        updateSelectizeInput(session,
                             "sampleAnnotationSlct",
                             choices = names(sample$annotation))
        updateSelectInput(session,
                          "sampleTypeSlct",
                          selected = sample$type$value)
        updateCheckboxInput(session,
                            "sampleInterRunCalibratorChk",
                            value = sample$interRunCalibrator)
        updateTextInput(session,
                        "sampleQuantityValueText",
                        value = testNull(sample$quantity$value))
        updateTextInput(session,
                        "sampleQuantityUnitText",
                        value = testNull(sample$quantity$unit$value))
        updateCheckboxInput(session,
                            "sampleCalibratorSampleChk",
                            value = sample$calibratorSample)
        updateTextInput(session,
                        "sampleCsmEnzymeText",
                        value = testNull(sample$cdnaSynthesisMethod$enzyme))
        updateTextInput(session,
                        "sampleCsmPrimingMethodSlct",
                        value = testNull(sample$cdnaSynthesisMethod$primingMethod$value))
        updateCheckboxInput(session,
                            "sampleCsmDnaseTreatmentChk",
                            value = sample$cdnaSynthesisMethod$dnaseTreatment)
        updateSelectInput(session,
                          "sampleCsmTccSlct",
                          selected = sample$cdnaSynthesisMethod$thermalCyclingConditions)
        updateTextInput(session,
                            "sampleTemplateQuantityConcText",
                            value = sample$templateQuantity$conc)
        updateSelectInput(session,
                          "sampleTemplateQuantityNucleotideSlct",
                          selected = sample$templateQuantity$nucleotide$value)
        
      } else {
        updateTextInput(session,
                        "sampleIdText",
                        value = input$sampleSlct)
      }
    })
  })
  
  updSampleRefs <- function() {
    updateSelectInput(session,
                      "reactSampleSlct",
                      choices = names(values$rdml$sample))
  }
  
  # write to sample
  observe({
    if (is.null(testEmptyInput(input$sampleIdText))) {
      return(NULL)
    }
    tryCatch({
      isolate({
        xRef <- values$rdml$sample[[input$sampleSlct]]$xRef
        annotation <- values$rdml$sample[[input$sampleSlct]]$annotation
      })
      sample <- sampleType$new(
        id = idType$new(testEmptyInput(input$sampleIdText)),
        description = testEmptyInput(input$sampleDescriptionText),
        documentation = 
          lapply(input$sampleDocumentationSlct,
                 function(doc) idReferencesType$new(doc)),
        xRef = xRef,
        annotation = annotation,
        type = 
          sampleTypeType$new(input$sampleTypeSlct),
        interRunCalibrator = testEmptyInput(input$sampleInterRunCalibratorChk),
        quantity = {
          if (input$sampleQuantityValueText == "") {
            NULL
          }
          else {
            tryCatch({
              quantityType$new(as.numeric(input$sampleQuantityValueText),
                               quantityUnitType$new(input$sampleQuantityUnitText))
            },
            error = function(e) {
              updLog(paste("sample quantity:", e$message))
              NULL}
            )}},
        calibratorSample = 
          testEmptyInput(input$sampleCalibratorSampleChk),
        # NULL,
        cdnaSynthesisMethod = {
          tryCatch({
            if (is.null(testEmptyInput(input$sampleCsmEnzymeText)) &&
                input$sampleCsmPrimingMethodSlct == "" &&
                input$sampleCsmDnaseTreatmentChk == FALSE &&
                input$sampleCsmTccSlct == ""
            ) {
              NULL
            } else {
              cdnaSynthesisMethodType$new(
                testEmptyInput(input$sampleCsmEnzymeText),
                {
                  if (input$sampleCsmPrimingMethodSlct == "")
                    NULL
                  else
                    primingMethodType$new(input$sampleCsmPrimingMethodSlct)
                },
                testEmptyInput(input$sampleCsmDnaseTreatmentChk),
                {
                  if (input$sampleCsmTccSlct == "")
                    NULL
                  else
                    idReferencesType$new(
                      testEmptyInput(input$sampleCsmTccSlct))
                })
            }
          },
          error = function(e) {
            updLog(paste("sample cdna :", e$message))
            NULL}
          )
        },
        templateQuantity = {
          tryCatch({
            if (input$sampleTemplateQuantityConcText == "")
              NULL
            else
              templateQuantityType$new(
                testEmptyInput(as.numeric(input$sampleTemplateQuantityConcText)),
                nucleotideType$new(testEmptyInput(input$sampleTemplateQuantityNucleotideSlct)))
          },
          error = function(e) {
            updLog(paste("sample template quantity:\n", e$message, "\n"))
            NULL}
          )
        }
      )
      isolate({
        values$rdml$sample[[input$sampleSlct]] <- sample
        # rename list elements
        if (input$sampleSlct != input$sampleIdText) {
          values$rdml$sample <- values$rdml$sample
          updateSelectizeInput(session,
                               "sampleSlct",
                               choices = names(values$rdml$sample),
                               selected = input$sampleIdText)
        }
        updSampleRefs()
      })
    },
    error = function(e) updLog(paste("sample:", e$message))
    )
  })
  
  # remove sample
  observe({
    input$removeSampleBtn
    isolate({
      values$rdml$sample[[input$sampleSlct]] <- NULL
      updateSelectizeInput(session,
                           "sampleSlct",
                           choices = names(values$rdml$sample))
      updSampleRefs()
    })
  })
  
  
  ###### annotation
  
  # on sampleAnnotationSlct change
  observe({
    if (input$sampleAnnotationSlct == "") {
      return(NULL)
    }
    isolate({
      # update fields
      if (!is.null(values$rdml$sample[[input$sampleSlct]]$
                   annotation[[input$sampleAnnotationSlct]])) {
        annotation <- values$rdml$sample[[input$sampleSlct]]$
          annotation[[input$sampleAnnotationSlct]]
        updateTextInput(session,
                        "sampleAnnotationPropertyText",
                        value = testNull(annotation$property))
        updateTextInput(session,
                        "sampleAnnotationValueText",
                        value = testNull(annotation$value))
      } else {
        updateTextInput(session,
                        "sampleAnnotationPropertyText",
                        value = input$sampleAnnotationSlct)
      }
    })
  })
  
  # write to sample annotation
  observe({
    if (is.null(testEmptyInput(input$sampleAnnotationPropertyText))) {
      return(NULL)
    }
    tryCatch({
      annotation <- annotationType$new(
        testEmptyInput(input$sampleAnnotationPropertyText),
        testEmptyInput(input$sampleAnnotationValueText))
      isolate({
        values$rdml$sample[[input$sampleSlct]]$
          annotation[[input$sampleAnnotationSlct]] <- annotation
        # rename list elements
        if (input$sampleAnnotationSlct != 
            input$sampleAnnotationPropertyText) {
          values$rdml$sample[[input$sampleSlct]]$
            annotation <- values$rdml$sample[[input$sampleSlct]]$
            annotation
          updateSelectizeInput(session,
                               "sampleAnnotationSlct",
                               choices = names(values$rdml$sample[[input$sampleSlct]]$
                                                 annotation),
                               selected = input$sampleAnnotationPropertyText)
        }
      })
    },
    error = function(e) updLog(e$message)
    )
  })
  
  # remove sample annotation
  observe({
    input$removeSampleAnnotationBtn
    isolate({
      values$rdml$sample[[input$sampleSlct]]$
        annotation[[input$sampleAnnotationSlct]]<- NULL
      updateSelectizeInput(
        session,
        "sampleAnnotationSlct",
        choices = names(values$rdml$sample[[input$sampleSlct]]$
                          annotation))
    })
  })
  
  ###### xRef
  
  # on samplexRefSlct change
  observe({
    if (input$samplexRefSlct == "") {
      return(NULL)
    }
    isolate({
      # update fields
      if (!is.null(values$rdml$sample[[input$sampleSlct]]$
                   xRef[[input$samplexRefSlct]])) {
        xRef <- values$rdml$sample[[input$sampleSlct]]$
          xRef[[input$samplexRefSlct]]
        updateTextInput(session,
                        "samplexRefNameText",
                        value = testNull(xRef$name))
        updateTextInput(session,
                        "samplexRefIdText",
                        value = testNull(xRef$id))
      } else {
        updateTextInput(session,
                        "samplexRefNameText",
                        value = input$samplexRefSlct)
      }
    })
  })
  
  # write to sample xRef
  observe({
    if (is.null(testEmptyInput(input$samplexRefNameText))) {
      return(NULL)
    }
    tryCatch({
      xRef <- xRefType$new(
        testEmptyInput(input$samplexRefNameText),
        testEmptyInput(input$samplexRefIdText))
      isolate({
        values$rdml$sample[[input$sampleSlct]]$
          xRef[[input$samplexRefSlct]] <- xRef
        # rename list elements
        if (input$samplexRefSlct != 
            input$samplexRefNameText) {
          values$rdml$sample[[input$sampleSlct]]$
            xRef <- values$rdml$sample[[input$sampleSlct]]$xRef
          updateSelectizeInput(
            session,
            "samplexRefSlct",
            choices = names(values$rdml$sample[[input$sampleSlct]]$xRef),
            selected = input$samplexRefNameText)
        }
      })
    },
    error = function(e) updLog(e$message)
    )
  })
  
  # remove sample xRef
  observe({
    input$removeSamplexRefBtn
    isolate({
      values$rdml$sample[[input$sampleSlct]]$
        xRef[[input$samplexRefSlct]]<- NULL
      updateSelectizeInput(
        session,
        "samplexRefSlct",
        choices = names(values$rdml$sample[[input$sampleSlct]]$
                          xRef))
    })
  })
  
  # Target Tab -----------------------------------------------------  
  
  
  # init
  observe({
    if (is.null(values$rdml$target))
      return(NULL)
    # updLog("Init Target\n")
    isolate({
      updateSelectizeInput(session,
                           "targetSlct",
                           choices = names(values$rdml$target))
    })
  })
  
  # on targetSlct change
  observe({
    if (input$targetSlct == "") {
      return(NULL)
    }
    isolate({
      if (length(values$rdml$target[[input$targetSlct]]$xRef) == 0) {
        updateSelectizeInput(
          session,
          "targetxRefSlct",
          choices = "")
      } else {
        updateSelectizeInput(
          session,
          "targetxRefSlct",
          choices = names(values$rdml$target[[input$targetSlct]]$xRef))
      }
      # update fields
      if (!is.null(values$rdml$target[[input$targetSlct]])) {
        target <- values$rdml$target[[input$targetSlct]]
        updateTextInput(session,
                        "targetIdText",
                        value = testNull(target$id$id))
        updateTextInput(session,
                        "targetDescriptionText",
                        value = testNull(target$description))
        updateSelectInput(session,
                          "targetDocumentationSlct",
                          selected = names(target$documentation))
        updateSelectizeInput(session,
                             "targetxRefSlct",
                             choices = names(target$xRef))
        updateSelectInput(session,
                          "targetTypeSlct",
                          selected = target$type$value)
        
        updateTextInput(session,
                        "targetAemText",
                        value = testNull(target$amplificationEfficiencyMethod))
        updateTextInput(session,
                        "targetAeText",
                        value = testNull(target$amplificationEfficiency))
        updateTextInput(session,
                        "targetAeSeText",
                        value = testNull(target$amplificationEfficiencySE))
        updateTextInput(session,
                        "targetDetectionLimitText",
                        value = testNull(target$detectionLimit))
        
        updateSelectInput(session,
                          "targetDyeIdSlct",
                          selected = target$dyeId$id)
        
        updateSelectInput(session,
                          "targetSequencesTypeSlct",
                          selected = "forwardPrimer")
        
        updateTextInput(session,
                        "targetCaCompanyText",
                        value = testNull(target$commercialAssay$company))
        updateTextInput(session,
                        "targetCaOrderNumberText",
                        value = testNull(target$commercialAssay$orderNumber))
        
      } else {
        updateTextInput(session,
                        "targetIdText",
                        value = input$targetSlct)
      }
    })
  })
  
  # on targetSequencesTypeSlct change
  observe({
    if (input$targetSequencesTypeSlct == "") {
      return(NULL)
    }
    isolate({
      
      olig <- values$rdml$target[[input$targetSlct]]$
        sequences[[input$targetSequencesTypeSlct]]
      updateTextInput(session,
                      "targetSequences3PrimeTagText",
                      value = testNull(olig$threePrimeTag))
      updateTextInput(session,
                      "targetSequences5PrimeTagText",
                      value = testNull(olig$fivePrimeTag))
      updateTextInput(session,
                      "targetSequencesSequenceText",
                      value = testNull(olig$sequence))
      
    })
  })
  
  #   updTargetRefs <- function() {
  #     updateSelectInput(session,
  #                       "dataTarSlct",
  #                       choices = names(values$rdml$target))
  #   }
  
  # write to target
  observe({
    if (is.null(testEmptyInput(input$targetIdText))) {
      return(NULL)
    }
    tryCatch({
      isolate({
        xRef <- values$rdml$target[[input$targetSlct]]$xRef
      })
      target <- targetType$new(
        idType$new(testEmptyInput(input$targetIdText)),
        testEmptyInput(input$targetDescriptionText),
        lapply(input$targetDocumentationSlct,
               function(doc) idReferencesType$new(testEmptyInput(doc))),
        xRef,
        
        targetTypeType$new(input$targetTypeSlct),
        testEmptyInput(input$targetAemText),
        testEmptyInput(as.numeric(input$targetAeText)),
        testEmptyInput(as.numeric(input$targetAeSeText)),
        testEmptyInput(as.numeric(input$targetDetectionLimitText)),
        
        dyeId = idReferencesType$new(testEmptyInput(input$targetDyeIdSlct)),
        
        {
          isolate({
            sequences <- values$rdml$target[[input$targetSlct]]$sequences
          })
          sequences[[input$targetSequencesTypeSlct]] <- 
            tryCatch({
              oligoType$new(
                input$targetSequences3PrimeTagText,
                input$targetSequences5PrimeTagText,
                input$targetSequencesSequenceText
              )
            },
            error = function(e) {
              updLog(e$message)
              NULL}
            )
          if (is.list(sequences)) {
            do.call(sequencesType$new, sequences)
          } else {
            NULL
          }
        },
        
        tryCatch({
          commercialAssayType$new(
            input$targetCaCompanyText,
            input$targetCaOrderNumberText
          )
        },
        error = function(e) {
          updLog(e$message)
          NULL}
        )
      )
      isolate({
        values$rdml$target[[input$targetSlct]] <- target
        # rename list elements
        if (input$targetSlct != input$targetIdText) {
          values$rdml$target <- values$rdml$target
          updateSelectizeInput(session,
                               "targetSlct",
                               choices = names(values$rdml$target),
                               selected = input$targetIdText)
        }
        # updTargetRefs()
      })
    },
    error = function(e) updLog(paste("target:", e$message))
    )
  })
  
  # remove target
  observe({
    input$removeTargetBtn
    isolate({
      values$rdml$target[[input$targetSlct]] <- NULL
      updateSelectizeInput(session,
                           "targetSlct",
                           choices = names(values$rdml$target))
      # updTargetRefs()
    })
  })
  
  ###### xRef
  
  # on targetxRefSlct change
  observe({
    if (input$targetxRefSlct == "") {
      return(NULL)
    }
    isolate({
      # update fields
      if (!is.null(values$rdml$target[[input$targetSlct]]$
                   xRef[[input$targetxRefSlct]])) {
        xRef <- values$rdml$target[[input$targetSlct]]$
          xRef[[input$targetxRefSlct]]
        updateTextInput(session,
                        "targetxRefNameText",
                        value = testNull(xRef$name))
        updateTextInput(session,
                        "targetxRefIdText",
                        value = testNull(xRef$id))
      } else {
        updateTextInput(session,
                        "targetxRefNameText",
                        value = input$targetxRefSlct)
      }
    })
  })
  
  # write to target xRef
  observe({
    if (is.null(testEmptyInput(input$targetxRefNameText))) {
      return(NULL)
    }
    tryCatch({
      xRef <- xRefType$new(
        testEmptyInput(input$targetxRefNameText),
        testEmptyInput(input$targetxRefIdText))
      isolate({
        values$rdml$target[[input$targetSlct]]$
          xRef[[input$targetxRefSlct]] <- xRef
        # rename list elements
        if (input$targetxRefSlct != 
            input$targetxRefNameText) {
          values$rdml$target[[input$targetSlct]]$
            xRef <- values$rdml$target[[input$targetSlct]]$xRef
          updateSelectizeInput(
            session,
            "targetxRefSlct",
            choices = names(values$rdml$target[[input$targetSlct]]$xRef),
            selected = input$targetxRefNameText)
        }
      })
    },
    error = function(e) updLog(e$message)
    )
  })
  
  # remove target xRef
  observe({
    input$removetargetxRefBtn
    isolate({
      values$rdml$target[[input$targetSlct]]$
        xRef[[input$targetxRefSlct]]<- NULL
      updateSelectizeInput(
        session,
        "targetxRefSlct",
        choices = names(values$rdml$target[[input$targetSlct]]$
                          xRef))
    })
  })
  
  # Tcc Tab -----------------------------------------------------  
  
  
  # init
  observe({
    if (is.null(values$rdml$thermalCyclingConditions))
      return(NULL)
    isolate({
      updateSelectizeInput(session,
                           "tccSlct",
                           choices = names(values$rdml$thermalCyclingConditions))
    })
  })
  
  # on tccSlct change
  observe({
    if (input$tccSlct == "") {
      return(NULL)
    }
    isolate({
      if (length(values$rdml$
                 thermalCyclingConditions[[input$tccSlct]]$step) == 0) {
        updateSelectizeInput(
          session,
          "tccStepSlct",
          choices = "")
      } else {
        updateSelectizeInput(
          session,
          "tccStepSlct",
          choices = names(values$rdml$
                            thermalCyclingConditions[[input$tccSlct]]$step))
      }
      # update fields
      if (!is.null(values$rdml$
                   thermalCyclingConditions[[input$tccSlct]])) {
        tcc <- values$rdml$
          thermalCyclingConditions[[input$tccSlct]]
        updateTextInput(session,
                        "tccIdText",
                        value = testNull(tcc$id$id))
        updateTextInput(session,
                        "tccDescriptionText",
                        value = testNull(tcc$description))
        updateSelectInput(session,
                          "tccDocumentationSlct",
                          selected = names(tcc$documentation))
        updateTextInput(session,
                        "tccLidTemperatureText",
                        value = testNull(tcc$lidTemperature))
        updateSelectInput(session,
                          "tccExperimenterSlct",
                          selected = names(tcc$experimenter))
        updateSelectizeInput(session,
                             "tccStepSlct",
                             choices = names(tcc$step))
        
        
      } else {
        updateTextInput(session,
                        "tccIdText",
                        value = input$tccSlct)
      }
    })
  })
  
  # on tccStepSlct change
  observe({
    if (input$tccStepSlct == "") {
      return(NULL)
    }
    isolate({
      step <- values$rdml$thermalCyclingConditions[[input$tccSlct]]$
        step[[input$tccStepSlct]]
      updateTextInput(session,
                      "tccStepNrText",
                      value = testNull(step$nr))
      updateTextInput(session,
                      "tccStepDescriptionText",
                      value = testNull(step$description))
      updateSelectInput(session,
                        "tccStepTypeSlct",
                        selected = {
                          if (!is.null(step$temperature))
                            "temperature"
                          else if ((!is.null(step$gradient)))
                            "gradient"
                          else if ((!is.null(step$loop)))
                            "loop"
                          else if ((!is.null(step$pause)))
                            "pause"
                          else if ((!is.null(step$lidOpen)))
                            "lidOpen"
                          else
                            "temperature"
                        })
      
      
      
    })
  })
  
  observe({
    if (input$tccStepTypeSlct == "" || input$tccStepSlct == "") {
      return(NULL)
    }
    isolate({
      step <- values$rdml$thermalCyclingConditions[[input$tccSlct]]$
        step[[input$tccStepSlct]]
      switch (input$tccStepTypeSlct,
              temperature = {
                updateTextInput(session,
                                "tccStepTemperatureText",
                                value = testNull(step$temperature$temperature))
                updateTextInput(session,
                                "tccStepDurationText",
                                value = testNull(step$temperature$duration))
                updateTextInput(session,
                                "tccStepTemperatureChangeText",
                                value = testNull(step$temperature$temperatureChange))
                updateTextInput(session,
                                "tccStepDurationChangeText",
                                value = testNull(step$temperature$durationChange))
                updateTextInput(session,
                                "tccStepMeasureText",
                                value = testNull(step$temperature$measure$value))
                updateTextInput(session,
                                "tccStepRampText",
                                value = testNull(step$temperature$ramp))
              },
              gradient = {
                updateTextInput(session,
                                "tccStepHighTemperatureText",
                                value = testNull(step$gradient$highTemperature))
                updateTextInput(session,
                                "tccStepLowTemperatureText",
                                value = testNull(step$gradient$lowTemperature))
                updateTextInput(session,
                                "tccStepDurationText",
                                value = testNull(step$gradient$duration))
                updateTextInput(session,
                                "tccStepTemperatureChangeText",
                                value = testNull(step$gradient$temperatureChange))
                updateTextInput(session,
                                "tccStepDurationChangeText",
                                value = testNull(step$gradient$durationChange))
                updateTextInput(session,
                                "tccStepMeasureText",
                                value = testNull(step$gradient$measure$value))
                updateTextInput(session,
                                "tccStepRampText",
                                value = testNull(step$gradient$ramp))
              },
              loop = {
                updateTextInput(session,
                                "tccStepGotoText",
                                value = testNull(step$loop$goto))
                updateTextInput(session,
                                "tccStepRepeatText",
                                value = testNull(step$loop$repeat.n))
              },
              pause = {
                updateTextInput(session,
                                "tccStepTemperatureText",
                                value = testNull(step$pause$temperature))
              },
              lidOpen = {
                if (is.null(step$lidOpen)) {
                  updateCheckboxInput(session,
                                      "tccStepLidOpenChk",
                                      value = FALSE)
                } else {
                  updateCheckboxInput(session,
                                      "tccStepLidOpenChk",
                                      value = TRUE)
                }
              },
              { 
                NULL
              }
      )
    })
  })
  
  updTccRefs <- function() {
    updateSelectInput(session,
                      "sampleCsmTccSlct",
                      choices = c("",
                                  names(values$rdml$thermalCyclingConditions)))
    updateSelectInput(session,
                      "runTccSlct",
                      choices = c("",
                                  names(values$rdml$thermalCyclingConditions)))
  }
  
  # write to tcc
  observe({
    if (is.null(testEmptyInput(input$tccIdText))) {
      return(NULL)
    }
    tryCatch({
      isolate({
        step <- values$rdml$thermalCyclingConditions[[input$tccSlct]]$step
      })
      tcc <- thermalCyclingConditionsType$new(
        id = idType$new(testEmptyInput(input$tccIdText)),
        description = testEmptyInput(input$tccDescriptionText),
        documentation = 
          lapply(input$tccDocumentationSlct,
                 function(doc) idReferencesType$new(doc)),
        lidTemperature = testEmptyInput(as.numeric(input$tccLidTemperatureText)),
        experimenter = lapply(input$tccExperimenterSlct,
                              function(exper) idReferencesType$new(exper)),
        step = step)
      
      isolate({
        values$rdml$thermalCyclingConditions[[input$tccSlct]] <- tcc
        # rename list elements
        if (input$tccSlct != input$tccIdText) {
          values$rdml$thermalCyclingConditions <- 
            values$rdml$thermalCyclingConditions
          updateSelectizeInput(session,
                               "tccSlct",
                               choices = names(values$rdml$thermalCyclingConditions),
                               selected = input$tccIdText)
        }
        updTccRefs()
      })},
      error = function(e) {
        updLog(paste("tcc:", e$message))
        NULL
      }
    )
    
  })
  
  # remove tcc
  observe({
    input$removeTccBtn
    isolate({
      values$rdml$thermalCyclingConditions[[input$tccSlct]] <- NULL
      updateSelectizeInput(session,
                           "tccSlct",
                           choices = names(values$rdml$thermalCyclingConditions))
      updTccRefs()
    })
  })
  
  ###### step
  
  
  
  # write to tcc step
  observe({
    if (is.null(testEmptyInput(input$tccStepNrText))) {
      return(NULL)
    }
    isolate({
      tccStepType <- input$tccStepTypeSlct
    })
    tryCatch({
      step <- stepType$new(
        testEmptyInput(as.numeric(input$tccStepNrText)),
        testEmptyInput(input$tccStepDescriptionText),
        temperature = {
          if (tccStepType == "temperature") {
            temperatureType$new(
              temperature = 
                testEmptyInput(as.numeric(input$tccStepTemperatureText)),
              duration = 
                testEmptyInput(as.numeric(input$tccStepDurationText)),
              temperatureChange = 
                testEmptyInput(as.numeric(input$tccStepTemperatureChangeText)),
              durationChange = 
                testEmptyInput(as.numeric(input$tccStepDurationChangeText)),
              measure = measureType$new(
                testEmptyInput(input$tccStepMeasureText)),
              ramp = 
                testEmptyInput(as.numeric(input$tccStepRampText))
            )
          } else { NULL }
        },
        gradient = {
          if (tccStepType == "gradient") {
            gradientType$new(
              highTemperature = 
                testEmptyInput(as.numeric(input$tccStepHighTemperatureText)),
              lowTemperature = 
                testEmptyInput(as.numeric(input$tccStepLowTemperatureText)),
              duration = 
                testEmptyInput(as.numeric(input$tccStepDurationText)),
              temperatureChange = 
                testEmptyInput(as.numeric(input$tccStepTemperatureChangeText)),
              durationChange = 
                testEmptyInput(as.numeric(input$tccStepDurationChangeText)),
              measure = measureType$new(
                testEmptyInput(input$tccStepMeasureText)),
              ramp = 
                testEmptyInput(as.numeric(input$tccStepRampText)))
          } else { NULL }
        },
        loop = {
          if (tccStepType == "loop") {
            loopType$new(
              goto = 
                testEmptyInput(as.numeric(input$tccStepGotoText)),
              repeat.n = 
                testEmptyInput(as.numeric(input$tccStepRepeatText))) 
          } else { NULL }
        },
        pause = {
          if (tccStepType == "pause") {
            pauseType$new(
              temperature = 
                testEmptyInput(as.numeric(input$tccStepTemperatureText)))
          } else { NULL }
        },
        lidOpen = {
          if (tccStepType == "lidOpen" &&
              input$tccStepLidOpenChk == TRUE) {
            lidOpenType$new()
          } else { NULL }
        })
      isolate({
        values$rdml$thermalCyclingConditions[[input$tccSlct]]$
          step[[input$tccStepSlct]] <- step
        # rename list elements
        if (input$tccStepSlct != input$tccStepNrText) {
          values$rdml$thermalCyclingConditions[[input$tccSlct]]$
            step <- values$rdml$thermalCyclingConditions[[input$tccSlct]]$step
          updateSelectizeInput(
            session,
            "targetxRefSlct",
            choices = names(values$rdml$
                              thermalCyclingConditions[[input$tccSlct]]$step),
            selected = input$tccStepNrText)
        }
      })
    },
    error = function(e) updLog(e$message)
    )
  })
  
  # remove tcc step
  observe({
    input$removeTccStepBtn
    isolate({
      values$rdml$thermalCyclingConditions[[input$tccSlct]]$
        step[[input$tccStepSlct]]<- NULL
      updateSelectizeInput(
        session,
        "tccStepSlct",
        choices = names(values$rdml$thermalCyclingConditions[[input$tccSlct]]$
                          step))
    })
  })
  
  # Experiment Tab -----------------------------------------------------  
  
  
  # init
  observe({
    if (is.null(values$rdml$experiment))
      return(NULL)
    # updLog("Init Experiment\n")
    isolate({
      updateSelectizeInput(session,
                           "experimentSlct",
                           choices = names(values$rdml$experiment))
    })
  })
  
  # on experimentSlct change
  observe({
    if (input$experimentSlct == "") {
      return(NULL)
    }
    isolate({
      if (length(values$rdml$
                 experiment[[input$experimentSlct]]$run) == 0) {
        updateSelectizeInput(
          session,
          "runSlct",
          choices = "")
      } else {
        updateSelectizeInput(
          session,
          "runSlct",
          choices = names(values$rdml$
                            experiment[[input$experimentSlct]]$run))
      }
      # update fields
      if (!is.null(values$rdml$
                   experiment[[input$experimentSlct]])) {
        experiment <- values$rdml$
          experiment[[input$experimentSlct]]
        updateTextInput(session,
                        "experimentIdText",
                        value = testNull(experiment$id$id))
        updateTextInput(session,
                        "experimentDescriptionText",
                        value = testNull(experiment$description))
        updateSelectInput(session,
                          "experimentDocumentationSlct",
                          selected = names(experiment$documentation))
        
        
      } else {
        updateTextInput(session,
                        "experimentIdText",
                        value = input$experimentSlct)
      }
      
      # updLog("update fields end\n")
    })
  })
  
  # on runSlct change
  observe({
    if (input$runSlct == "") {
      return(NULL)
    }
    # updLog("On runSlct change\n")
    isolate({
      if (length(values$rdml$
                 experiment[[input$experimentSlct]]$
                 run[[input$runSlct]]$react) == 0) {
        updateSelectizeInput(
          session,
          "reactSlct",
          choices = "")
      } else {
        updateSelectizeInput(
          session,
          "reactSlct",
          choices = names(values$rdml$
                            experiment[[input$experimentSlct]]$
                            run[[input$runSlct]]$react))
      }
      # update fields
      if (!is.null(values$rdml$
                   experiment[[input$experimentSlct]]$
                   run[[input$runSlct]])) {
        run <- values$rdml$
          experiment[[input$experimentSlct]]$
          run[[input$runSlct]]
        updateTextInput(session,
                        "runIdText",
                        value = testNull(run$id$id))
        updateTextInput(session,
                        "runDescriptionText",
                        value = testNull(run$description))
        updateSelectInput(session,
                          "runDocumentationSlct",
                          selected = names(run$documentation))
        updateSelectInput(session,
                          "runExperimenterSlct",
                          selected = names(run$experimenter))
        updateTextInput(session,
                        "runInstrumentText",
                        value = testNull(run$instrument))
        
        updateTextInput(session,
                        "runDataCollectionSoftwareNameText",
                        value = testNull(run$dataCollectionSoftware$name))
        updateTextInput(session,
                        "runDataCollectionSoftwareVersionText",
                        value = testNull(run$dataCollectionSoftware$version))
        
        updateTextInput(session,
                        "runBackgroundDeterminationMethodText",
                        value = testNull(run$backgroundDeterminationMethod))
        
        updateSelectInput(session,
                          "runCqDetectionMethodSlct",
                          selected = names(run$cqDetectionMethod$value))
        updateSelectInput(session,
                          "runTccSlct",
                          selected = names(run$thermalCyclingConditions))
        
        updateTextInput(session,
                        "runRowsText",
                        value = testNull(run$pcrFormat$rows))
        updateTextInput(session,
                        "runColumnsText",
                        value = testNull(run$pcrFormat$columns))
        updateSelectInput(session,
                          "runRowLabelSlct",
                          selected = names(run$pcrFormat$rowLabel$value))
        updateSelectInput(session,
                          "runColumnLabelSlct",
                          selected = names(run$pcrFormat$columnLabel$value))
        
        updateTextInput(session,
                        "runDateText",
                        value = testNull(run$runDate))
        
        
      } else {
        updateTextInput(session,
                        "runIdText",
                        value = input$runSlct)
      }
    })
  })
  
  
  # on reactSlct change
  observe({
    if (input$reactSlct == "") {
      return(NULL)
    }
    isolate({
      if (length(values$rdml$
                 experiment[[input$experimentSlct]]$
                 run[[input$runSlct]]$
                 react[[input$reactSlct]]) == 0) {
        updateSelectizeInput(
          session,
          "dataSlct",
          choices = "")
      } else {
        updateSelectizeInput(
          session,
          "dataSlct",
          choices = names(values$rdml$
                            experiment[[input$experimentSlct]]$
                            run[[input$runSlct]]$
                            react[[input$reactSlct]]$data))
      }
      # update fields
      if (!is.null(values$rdml$
                   experiment[[input$experimentSlct]]$
                   run[[input$runSlct]]$
                   react[[input$reactSlct]])) {
        react <- values$rdml$
          experiment[[input$experimentSlct]]$
          run[[input$runSlct]]$
          react[[input$reactSlct]]
        updateTextInput(session,
                        "reactIdText",
                        value = testNull(react$id$id))
        updateSelectInput(session,
                          "reactSampleSlct",
                          selected = react$sample$id)
      } else {
        updateTextInput(session,
                        "reactIdText",
                        value = input$reactSlct)
      }
    })
  })
  
  
  # on dataSlct change
  observe({
    if (input$dataSlct == "") {
      return(NULL)
    }
    isolate({
      
      # update fields
      if (!is.null(values$rdml$
                   experiment[[input$experimentSlct]]$
                   run[[input$runSlct]]$
                   react[[input$reactSlct]]$
                   data[[input$dataSlct]])) {
        data <- values$rdml$
          experiment[[input$experimentSlct]]$
          run[[input$runSlct]]$
          react[[input$reactSlct]]$
          data[[input$dataSlct]]
        
        updateSelectInput(session,
                          "dataTarSlct",
                          choices = names(values$rdml$target),
                          selected = data$tar$id)
        updateTextInput(session,
                        "dataCqText",
                        value = testNull(data$cq))
        updateTextInput(session,
                        "dataExclText",
                        value = testNull(data$excl))
        updateTextInput(session,
                        "dataEndPtText",
                        value = testNull(data$endPt))
        updateTextInput(session,
                        "dataBgFluorText",
                        value = testNull(data$bgFluor))
        updateTextInput(session,
                        "dataBgFluorSlpText",
                        value = testNull(data$bgFluorSlp))
        updateTextInput(session,
                        "dataQuantFluorText",
                        value = testNull(data$quantFluor))
      } else {
        updateTextInput(session,
                        "dataTarText",
                        value = input$dataSlct)
      }
    })
  })
  
  # write to experiment
  observe({
    if (is.null(testEmptyInput(input$experimentIdText))) {
      return(NULL)
    }
    # updLog("Write to experiment")
    tryCatch({
      isolate({
        run <- values$rdml$
          experiment[[input$experimentSlct]]$run
      })
      experiment <- experimentType$new(
        id = idType$new(testEmptyInput(input$experimentIdText)),
        description = testEmptyInput(input$experimentDescriptionText),
        documentation = 
          lapply(input$experimentDocumentationSlct,
                 function(doc) idReferencesType$new(doc)),
        run = run)
      
      isolate({
        values$rdml$experiment[[input$experimentSlct]] <- experiment
        # rename list elements
        if (input$experimentSlct != input$experimentIdText) {
          values$rdml$experiment <- 
            values$rdml$experiment
          updateSelectizeInput(session,
                               "experimentSlct",
                               choices = names(values$rdml$experiment),
                               selected = input$experimentIdText)
        }
      })},
      error = function(e) {
        updLog(paste("experiment:", e$message))
        NULL
      }
    )
    
  })
  
  # write to run
  observe({
    if (is.null(testEmptyInput(input$runIdText))) {
      return(NULL)
    }
    tryCatch({
      isolate({
        react <- values$rdml$
          experiment[[input$experimentSlct]]$
          run[[input$runSlct]]$react
      })
      run <- runType$new(
        id = idType$new(testEmptyInput(input$runIdText)),
        description = testEmptyInput(input$runDescriptionText),
        documentation = 
          lapply(input$runDocumentationSlct,
                 function(doc) idReferencesType$new(doc)),
        experimenter = 
          lapply(input$runExperimenterSlct,
                 function(experimenter) idReferencesType$new(experimenter)),
        instrument = testEmptyInput(input$runInstrumentText),
        dataCollectionSoftware = 
          dataCollectionSoftwareType$new(
            testEmptyInput(input$runDataCollectionSoftwareNameText),
            testEmptyInput(input$runDataCollectionSoftwareVersionText)),
        testEmptyInput(input$runBackgroundDeterminationMethodText),
        cqDetectionMethodType$new(
          testEmptyInput(input$runCqDetectionMethodSlct)),
        tryCatch(
          idReferencesType$new(
            testEmptyInput(input$runTccSlct)),
          error = function(e) return(NULL)),
        pcrFormat = pcrFormatType$new(
          rows = testEmptyInput(as.numeric(input$runRowsText)),
          columns = testEmptyInput(as.numeric(input$runColumnsText)),
          rowLabel = 
            labelFormatType$new(testEmptyInput(input$runRowLabelSlct)),
          columnLabel = 
            labelFormatType$new(testEmptyInput(input$runColumnLabelSlct))),
        runDate = testEmptyInput(input$runDateText),
        react = react
      )
      
      isolate({
        values$rdml$experiment[[input$experimentSlct]]$
          run[[input$runSlct]] <- run
        # rename list elements
        if (input$runSlct != input$runIdText) {
          values$rdml$experiment[[input$experimentSlct]]$
            run <- values$rdml$experiment[[input$experimentSlct]]$run
          updateSelectizeInput(session,
                               "experimentSlct",
                               choices = names(
                                 values$rdml$experiment[[input$experimentSlct]]$
                                   run
                               ),
                               selected = input$runIdText)
        }
      })},
      error = function(e) {
        updLog(paste("run:", e$message))
        NULL
      }
    )
    
  })
  
  # write to react
  observe({
    if (is.null(testEmptyInput(input$reactIdText))) {
      return(NULL)
    }
    tryCatch({
      isolate({
        data <- values$rdml$
          experiment[[input$reactSampleSlct]]$
          run[[input$runSlct]]$
          react[[input$reactSlct]]$data
      })
      react <- reactType$new(
        id = reactIdType$new(
          as.numeric(testEmptyInput(input$reactIdText))),
        sample = idReferencesType$new(
          testEmptyInput(input$runExperimenterSlct)),
        data = data
      )
      
      isolate({
        values$rdml$experiment[[input$experimentSlct]]$
          run[[input$runSlct]]$
          react[[input$reactSlct]] <- react
        # rename list elements
        if (input$reactSlct != input$reactIdText) {
          values$rdml$experiment[[input$experimentSlct]]$
            run[[input$runSlct]]$
            react <- values$rdml$experiment[[input$experimentSlct]]$
            run[[input$runSlct]]$
            react
          updateSelectizeInput(session,
                               "reactSlct",
                               choices = names(
                                 values$rdml$experiment[[input$experimentSlct]]$
                                   run[[input$runSlct]]$
                                   react
                               ),
                               selected = input$reactIdText)
        }
      })},
      error = function(e) {
        updLog(paste("react:", e$message))
        NULL
      }
    )
  })
  
  
  # write to data
  observe({
    if (is.null(testEmptyInput(input$dataTarSlct))) {
      return(NULL)
    }
    tryCatch({
      isolate({
        adp <- values$rdml$
          experiment[[input$experimentSlct]]$
          run[[input$runSlct]]$
          react[[input$reactSlct]]$
          data[[input$dataTarSlct]]$adp
        mdp <- values$rdml$
          experiment[[input$experimentSlct]]$
          run[[input$runSlct]]$
          react[[input$reactSlct]]$
          data[[input$dataTarSlct]]$mdp
      })
      data <- dataType$new(
        tar = idReferencesType$new(testEmptyInput(input$dataTarSlct)),
        cq = testEmptyInput(as.numeric(input$dataCqText)),
        excl = testEmptyInput(input$dataExclText),
        adp = adp,
        mdp = mdp,
        endPt = testEmptyInput(as.numeric(input$dataEndPtText)),
        bgFluor = testEmptyInput(as.numeric(input$dataBgFluorText)),
        bgFluorSlp = testEmptyInput(as.numeric(input$dataBgFluorSlpText)),
        quantFluor = testEmptyInput(as.numeric(input$dataQuantFluorText))
      )
      
      isolate({
        values$rdml$experiment[[input$experimentSlct]]$
          run[[input$runSlct]]$
          react[[input$reactSlct]]$
          data[[input$dataTarSlct]] <- data
        # rename list elements
        if (input$dataSlct != input$dataTarSlct) {
          values$rdml$experiment[[input$experimentSlct]]$
            run[[input$runSlct]]$
            react[[input$reactSlct]]$data <- 
            values$rdml$experiment[[input$experimentSlct]]$
            run[[input$runSlct]]$
            react[[input$reactSlct]]$data
          updateSelectizeInput(session,
                               "dataSlct",
                               choices = names(
                                 values$rdml$experiment[[input$experimentSlct]]$
                                   run[[input$runSlct]]$
                                   react[[input$reactSlct]]$data
                               ),
                               selected = input$dataSlct)
        }
      })},
      error = function(e) {
        updLog(paste("data:", e$message))
        NULL
      }
    )
    
  })
  
  # remove experiment
  observe({
    input$removeExperimentBtn
    isolate({
      values$rdml$experiment[[input$experimentSlct]] <- NULL
      updateSelectizeInput(session,
                           "experimentSlct",
                           choices = names(values$rdml$experiment))
    })
  })
  
  # remove run
  observe({
    input$removeRunBtn
    isolate({
      values$rdml$experiment[[input$experimentSlct]]$
        run[[input$runSlct]] <- NULL
      updateSelectizeInput(session,
                           "runSlct",
                           choices = names(values$rdml$experiment$
                                             run))
    })
  })
  
  # remove react
  observe({
    input$removeReactBtn
    isolate({
      values$rdml$experiment[[input$experimentSlct]]$
        run[[input$runSlct]]$
        react[[input$reactSlct]] <- NULL
      updateSelectizeInput(session,
                           "reactSlct",
                           choices = names(values$rdml$experiment[[input$experimentSlct]]$
                                             run[[input$runSlct]]$
                                             react))
    })
  })
  
  # remove data
  observe({
    input$removeDataBtn
    isolate({
      values$rdml$experiment[[input$experimentSlct]]$
        run[[input$runSlct]]$
        react[[input$reactSlct]]$
        data[[input$dataSlct]] <- NULL
      updateSelectizeInput(session,
                           "dataSlct",
                           choices = names(values$rdml$experiment[[input$experimentSlct]]$
                                             run[[input$runSlct]]$
                                             react[[input$reactSlct]]$
                                             data))
    })
  })
  
  # Calculations ------------------------------------------------------------
  preprocessAdpDone <- reactive({
    if (is.null(values$rdml))
      return(NULL)
    # cat("Calculations\n")
    tbl <- values$rdml$AsTable()
    smooth <- TRUE
    smooth.method <- input$smoothqPCRmethod
    if (input$smoothqPCRmethod == "none") {
      smooth <- FALSE
      smooth.method <- "savgol"
    }
    if (input$preprocessqPCR) {
      tbl[adp == TRUE,
          {
            values$rdml$experiment[[exp.id]]$
              run[[run.id]]$
              react[[as.character(react.id)]]$
              data[[target]]$PreprocessAdp(smooth,
                                           smooth.method,
                                           input$normqPCRmethod)
          }, by = fdata.name]
    } else {
      tbl[adp == TRUE,
          {
            values$rdml$experiment[[exp.id]]$
              run[[run.id]]$
              react[[as.character(react.id)]]$
              data[[target]]$UndoPreprocessAdp()
          }, by = fdata.name]
    }
    exp.ids <- unique(tbl$exp.id)
    updateSelectInput(session,
                      "showqPCRExperiment",
                      choices = exp.ids,
                      selected = exp.ids[1])
    runif(1)
  })
  
  observe({
    req(input$showqPCRExperiment)
    run.ids <- unique(values$rdml$AsTable()[exp.id == input$showqPCRExperiment, run.id])
    updateSelectInput(session,
                      "showqPCRRun",
                      choices = run.ids,
                      selected = run.ids[1])
  })
  
  output$thLevelsUI <- renderUI({
    if (is.null(values$rdml))
      return(NULL)
    tbl <- values$rdml$AsTable()
    wellPanel(
      list.map(unique(tbl$target),
               target ~ {
                 numericInput(paste0("thLevel_", target),
                              HTML(sprintf("Threshold <b>%s</b>", target)),
                              0, 0, step = 0.01)
               })
    )
  })
  
  cqCalcsDone <- reactive({
    req(preprocessAdpDone())
    tbl <- values$rdml$AsTable()
    tbl[adp == TRUE,
        {
          values$rdml$experiment[[exp.id]]$
            run[[run.id]]$
            react[[as.character(react.id)]]$
            data[[target]]$CalcCq(input$cqMethod,
                                  input[[paste0("thLevel_", target)]],
                                  input$autoThLevel)
        }, by = fdata.name]
  })
  
  preprocessMdpDone <- reactive({
    if (is.null(values$rdml))
      return(NULL)
    # cat("Calculations MDP\n")
    tbl <- values$rdml$AsTable()
    if (input$preprocessMelting) {
      tbl[mdp == TRUE,
          {
            values$rdml$experiment[[exp.id]]$
              run[[run.id]]$
              react[[as.character(react.id)]]$
              data[[target]]$PreprocessMdp(input$bgAdjMelting,
                                           input$bgRangeMelting,
                                           input$minMaxMelting,
                                           input$dfFactMelting)
          }, by = fdata.name]
    } else {
      tbl[mdp == TRUE,
          {
            values$rdml$experiment[[exp.id]]$
              run[[run.id]]$
              react[[as.character(react.id)]]$
              data[[target]]$UndoPreprocessMdp()
          }, by = fdata.name]
    }
    exp.ids <- unique(tbl$exp.id)
    updateSelectInput(session,
                      "showMeltingExperiment",
                      choices = exp.ids,
                      selected = exp.ids[1])
    runif(1)
  })
  
  observe({
    req(input$showMeltingExperiment)
    run.ids <- unique(values$rdml$AsTable()[exp.id == input$showMeltingExperiment, run.id])
    updateSelectInput(session,
                      "showMeltingRun",
                      choices = run.ids,
                      selected = run.ids[1])
  })
  # Curves and table --------------------------------------------------------
  
  rdmlTable <- reactive({
    # if (is.null(values$rdml) || !(input$mainNavbar %in% c("adp","mdp")))
    if (is.null(cqCalcsDone()) || 
        is.null(preprocessMdpDone()) ||
        !(input$mainNavbar %in% c("adp","mdp")))
      return(NULL)
    # updLog("Create rdmlTable")
    input$showqPCRExperiment
    input$showqPCRRun
    input$showMeltingExperiment
    input$showMeltingRun
    # cat("Create rdmlTable")
    isolate({
      tbl <- values$rdml$AsTable(
        add.columns = list(
          cq = {
            cq <- data$cq
            if (is.null(cq))
              as.numeric(NA)
            else
              cq
          },
          quantFluor = {
            quantFluor <- data$quantFluor
            if (is.null(quantFluor)) {
              if (is.null(cq)) {
                as.numeric(NA)
              } else {
                0
              }
            } else {
              quantFluor
            }
          })
      )[get(input$mainNavbar) == TRUE &
          exp.id == input$showqPCRExperiment &
          run.id == input$showqPCRRun, !c("adp", "mdp")][
        , c("cq.mean", "cq.sd", "quantFluor.mean") := list(
          mean(cq, na.rm = TRUE),
          sd(cq, na.rm = TRUE),
          mean(quantFluor, na.rm = TRUE)
        ),
        by = .(sample, target)]
      if (nrow(tbl) == 0) {
        return(NULL)
      }
      values$lockReplot <- 0
      values$selectedTubes <-
        data.frame(position = tbl$position %>>% unique(),
                   selected = FALSE,
                   stringsAsFactors = FALSE) %>>%
        mutate(row = str_extract(position, "[A-Z]") %>>% factor(levels = rev(LETTERS[1:values$rdml$experiment[[input$showqPCRExperiment]]$
                                                                                       run[[input$showqPCRRun]]$pcrFormat$rows])),
               column = str_extract(position, "[0-9]+") %>>% as.numeric() %>>% as.character())
      rownames(values$selectedTubes) <- values$selectedTubes$position
      targets <- unique(tbl$target)
      updateSelectInput(session,
                        paste0("showTargets", input$mainNavbar),
                        choices = targets,
                        selected = targets)
      tbl
    })
  })
  
  # plate
  plateHTML <- reactive({
    req(rdmlTable())
    # cat("Redraw plate\n")
    isolate({
      tbl <- rdmlTable()
      tbl[1, 1] <- tbl[1, 1]
      cell.template <- paste0("<td id='tube_{{position}}'",
                              "title='{{position}}\u000A{{sample}}' ",
                              "class='{{class}}' ",
                              "style='background-color:rgba({{bgcolor}}, 0.35)'>")#,
      #"{{snamef}}</td>")
      descr <- tbl %>>%
        group_by(position) %>>%
        summarise_each(funs(first)) %>>%
        # left_join(calc.Cqs(c("tr"), values$preprocessed$tr %>>% names),
        #           by = "position") %>>%
        left_join(values$selectedTubes
                  , by = "position") %>>%
        group_by(fdata.name) %>>%
        mutate(snamef = format.smpl.name(sample, 5),
               class = paste(ifelse(selected,
                                    "sel",
                                    "")),
               bgcolor = "red") %>>%
        as.data.frame()
      rownames(descr) <- descr$position
      pcr.format <- values$rdml$experiment[[input$showqPCRExperiment]]$
        run[[input$showqPCRRun]]$pcrFormat
      sprintf(paste0('<table id="plateTable" class="plateTable"><thead>',
                     '<tr><th id="toggleall" class="br-triangle"></th>%s</tr></thead>',
                     '<tbody>%s</tbody></table>',
                     '<script>addCellOnClick();</script>'),
              list.map(1:pcr.format$columns, col ~ sprintf("<th id='col_%02i'>%s</th>", col, col)) %>>%
                paste(collapse = ""),
              list.map(LETTERS[1:pcr.format$rows],
                       row ~ sprintf("<tr><th id='row_%s' class='%s'>%s</th>%s</tr>", row,
                                     {
                                       if (as.integer(charToRaw(row)) %% 2 == 0) "even-row"
                                       else "odd-row"
                                     },
                                     row,
                                     list.map(1:pcr.format$columns,
                                              col ~ {
                                                tube <- sprintf("%s%02i", row, col)
                                                if (is.na(descr[tube, "fdata.name"]))
                                                  return("<td></td>")
                                                paste0(
                                                  whisker.render(cell.template,
                                                                 descr[tube, ]),
                                                  descr[tube, "snamef"],
                                                  "</td>"
                                                )
                                              }) %>>%
                                       paste(collapse = ""))
              ) %>>%
                paste(collapse = "")) %>>%
        HTML
    })
  })
  
  output$plateTbl <- renderUI({
    req(plateHTML())
    plateHTML()
  })
  
  output$plateTblMelting <- renderUI({
    req(plateHTML())
    plateHTML()
  })
  
  # plate click
  observe({
    req(input$cellClick)
    isolate({
      values$lockReplot <- values$lockReplot + 1
      # row <- input$cellClick[2]
      # col <- input$cellClick[3]
      id <- str_split(input$cellClick[2], "_")[[1]]
      dblclick <- ifelse(input$cellClick[3] == "dblclick",
                         TRUE, FALSE)
      ctrl <- ifelse(input$cellClick[4] == "ctrl",
                     TRUE, FALSE)
      toggle <- function(action, positions) {
        for (position in positions) {
          action(paste("tube", position, sep = "_"), "sel", NULL)
        }
      }
      selectPositions <- function(positions) {
        if (length(positions) == 0)
          return(NULL)
        selected <- values$selectedTubes
        if (dblclick) {
          selected$selected <- FALSE
          toggle(removeClass, values$selectedTubes$position)
        }
        if (all(values$selectedTubes[positions, "selected"] == TRUE)) {
          selected[positions, "selected"] <- FALSE
          toggle(removeClass, positions)
        } else {
          selected[positions, "selected"] <- TRUE
          toggle(addClass, positions)
        }
        values$selectedTubes <- selected
      }
      switch(id[1],
             "tube" = {
               positions <- {
                 if (ctrl) {
                   descr <- rdmlTable()
                   smpl <- descr[descr$position == id[2], sample]
                   target <- descr[descr$position == id[2], target]
                   descr[descr$sample == smpl & descr$target == target, position]
                 } else {
                   id[2]
                 }}
               selectPositions(positions)
             },
             "toggleall" = {
               if (all(values$selectedTubes$selected == TRUE) || dblclick) {
                 values$selectedTubes$selected <- FALSE
                 toggle(removeClass, values$selectedTubes$position)
               } else {
                 values$selectedTubes$selected <- TRUE
                 toggle(addClass, values$selectedTubes$position)
               }
             },
             "col" = {
               positions <- str_extract(values$selectedTubes$position,
                                        paste0("[A-Z]",
                                               id[2])) %>>%
                 na.omit
               selectPositions(positions)
             },
             "row" = {
               positions <- str_extract(values$selectedTubes$position,
                                        paste0(id[2],
                                               "[0-9]+")) %>>%
                 na.omit()
               selectPositions(positions)
             },
             {})
      values$lockReplot <- values$lockReplot - 1
    })
  })
  
  # filter qPCRDt
  observe({
    req(values$selectedTubes)
    positions <- values$selectedTubes %>>%
      filter(selected == TRUE) %>>%
      (position)
    if (length(positions) == 0)
      positions <- values$selectedTubes$position
    js$filterTblByPositions(
      paste(positions, collapse = "|"))
  })
  
  fdata <- reactive({
    req(rdmlTable())
    # updLog("Create fdata")
    tbl <- rdmlTable()
    fdata <- values$rdml$GetFData(tbl, 
                         dp.type = input$mainNavbar,
                         long.table = TRUE)
    if (input$mainNavbar == "mdp"){
      fdata[
        , fluor.deriv := {
          diff <- diffQ(data.frame(tmp, fluor), verbose = TRUE, warn = FALSE)$xy
          c(head(diff, 1)[1, 2], diff[, 2], tail(diff, 1)[1, 2])
        }, by = fdata.name]
    }
    fdata
  })
  
  fdata.filtered <- reactive({
    req(fdata())
    if (values$lockReplot != 0)
      return(NULL)
    fd <- fdata()[target %in% input[[paste0("showTargets", input$mainNavbar)]]]
    if (!all(values$selectedTubes$selected == FALSE)) {
      fd <- fd[
        position %in% values$selectedTubes$position[values$selectedTubes$selected == TRUE]]
    }
    if (nrow(fd) == 0)
      return(NULL)
    fd
  })
  
  output$qPCRPlot <- renderPlotly({
    if (is.null(fdata.filtered()) || input$mainNavbar == "mdp")
      return(NULL)
    
    fpoints <- fdata.filtered()
    # just to copy in memory to new table
    fpoints[1, 1] <- fpoints[1, 1]
    # plot ticks setup
    if (input$logScale) {
      fpoints[, c("fluor", "quantFluor", "quantFluor.mean") := 
                list(
                  {
                    new.fluor <- log2(fluor)
                    ifelse(is.nan(new.fluor) | new.fluor == -Inf,
                           NA,
                           new.fluor)},
                  {
                    new.fluor <- log2(quantFluor)
                    ifelse(is.nan(new.fluor) | new.fluor == -Inf,
                           0,
                           new.fluor)},
                  {
                    new.fluor <- log2(quantFluor.mean)
                    ifelse(is.nan(new.fluor) | new.fluor == -Inf,
                           0,
                           new.fluor)}
                )]
    }
    p <- plot_ly(fpoints %>>% 
              group_by(fdata.name),
            x = ~cyc,
            y = ~fluor,
            type = "scatter",
            mode = "lines",
            color = {
              if (input$colorqPCRby == "none")
                ""
              else
                fpoints[, get(input$colorqPCRby)]
            },
            linetype = {
              if (input$shapeqPCRby == "none")
                NULL
              else
                fpoints[, get(input$shapeqPCRby)]
            })
    p <- switch(input$showCq,
                none = p,
                yes = add_trace(p,
                                x = ~cq,
                                y = ~quantFluor,
                                type = "scatter",
                                mode = "markers",
                                color = {
                                  if (input$colorqPCRby == "none")
                                    ""
                                  else
                                    fpoints[, get(input$colorqPCRby)]
                                },
                                symbol = {
                                  if (input$shapeqPCRby == "none")
                                    NULL
                                  else
                                    fpoints[, get(input$shapeqPCRby)]
                                },
                                inherit = FALSE),
                mean = add_trace(p,
                                 data = fpoints %>>% 
                                   group_by(sample, target),
                                 x = ~cq.mean,
                                 y = ~quantFluor.mean,
                                 type = "scatter",
                                 mode = "markers",
                                 color = {
                                   if (input$colorqPCRby == "none")
                                     ""
                                   else
                                     fpoints[, get(input$colorqPCRby)]
                                 },
                                 symbol = {
                                   if (input$shapeqPCRby == "none")
                                     NULL
                                   else
                                     fpoints[, get(input$shapeqPCRby)]
                                 },
                                 inherit = FALSE)
    )
    p
  })
  
  output$qPCRDt <- renderDataTable({
    if (is.null(rdmlTable())) 
      return(NULL)
    # input$recalculateCq
    # isolate({
      tbl <- rdmlTable()[, !"quantFluor.mean"]
      names(tbl)[1] <- "data.name"
      tbl
  }
  # ,
  # callback = "function(table) {
  #   table.on('click.dt', 'tr', function() {
  #   $(this).toggleClass('selected');
  #   Shiny.onInputChange('selectedRows',
  #   table.rows('.selected').data().toArray());
  #   });
  #   }"
  )
  
  # melting
  output$meltingPlot <- renderPlotly({
    if (is.null(fdata.filtered()) || input$mainNavbar == "adp")
      return(NULL)
    p <- plot_ly(data = fdata.filtered() %>>% group_by(fdata.name),
                 x = ~tmp,
                 color = {
                   if (input$colorMeltingBy == "none")
                     ""
                   else
                     fdata.filtered()[, get(input$colorMeltingBy)]
                 },
                 linetype = {
                   if (input$shapeMeltingBy == "none")
                     NULL
                   else
                     fdata.filtered()[, get(input$shapeMeltingBy)]
                 })
    subplot(
      add_lines(p,
                y = ~fluor),
      add_lines(p,
                y = ~fluor.deriv),
      shareX = TRUE
    )
  })
  
  output$meltingDt <- renderDataTable({
    if (is.null(rdmlTable())) 
      return(NULL)
    tbl <- rdmlTable()[, !c("quantFluor", "quantFluor.mean", "cq", "cq.mean", "cq.sd")]
    names(tbl)[1] <- "data.name"
    tbl
  }
  # ,callback = "function(table) {
  #   table.on('click.dt', 'tr', function() {
  #   $(this).toggleClass('selected');
  #   Shiny.onInputChange('selectedRows',
  #   table.rows('.selected').data().toArray());
  #   });
  #   }"
  )
  
  # Download ----------------------------------------------------------------
  
  output$downloadRDML <- downloadHandler(
    filename = function() {
      paste0(input$rdmlFileSlct, ".rdml")
    },
    content = function(file) {
      output <- values$rdml$AsXML(file)
    }
  )
  
  # Log ---------------------------------------------------------------------
  
  
  output$logText <- renderUI({
    if (is.null(values$log))
      return(NULL)
    HTML(paste(values$log, collapse = "<br>"))
  })
  
  observe({
    input$clearLogBtn
    isolate({
      values$log <- ""
    })
  })
  
})

