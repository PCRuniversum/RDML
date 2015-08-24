
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RDML)
library(dplyr)

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
    tryCatch({
      values$rdml$dateMade <- testEmptyInput(input$dateMadeText)
      values$rdml$dateUpdated <- testEmptyInput(input$dateUpdatedText)
    },
    error = function(e) print(e))
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
  
  
  
  # Documentation Tab -----------------------------------------------------  
  
  # init
  observe({
    if (is.null(values$rdml$documentation))
      return(NULL)
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
        #         updateTextInput(session,
        #                         "documentationTextText",
        #                         value = testNull(documentation$text))
      } else {
        updateTextInput(session,
                        "documentationIdText",
                        value = input$documentationSlct)
      }
    })
  })
  
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
        updateSelectInput(session,
                          "sampleDocumentationSlct",
                          choices = names(values$rdml$documentation))
        updateSelectInput(session,
                          "targetDocumentationSlct",
                          choices = names(values$rdml$documentation))
      })
    },
    error = function(e) print(e$message)
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
      updateSelectInput(session,
                        "sampleDocumentationSlct",
                        choices = names(values$rdml$documentation))
      updateSelectInput(session,
                        "targetDocumentationSlct",
                        choices = names(values$rdml$documentation))
    })
  })
  
  # Dye Tab -----------------------------------------------------  
  
  
    # init
    observe({
      if (is.null(values$rdml$dye))
        return(NULL)
      isolate({
        updateSelectizeInput(session,
                             "dyeSlct",
                             choices = names(values$rdml$dye))
        updateSelectInput(session,
                          "targetDyeIdSlct",
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
    

    
      # write to dye
      dyes <- reactive({
        if (is.null(testEmptyInput(input$dyeIdText)) ||
            is.null(input$dyeSlct)) {
          return(dyes)
        }
        tryCatch({
          tdyes <- dyes()
          tdyes[[input$dyeSlct]] <- dyeType$new(
            idType$new(testEmptyInput(input$dyeIdText)),
            testEmptyInput(input$dyeDescriptionText))
          isolate({
            # values$rdml$dye[[input$dyeSlct]] <- dye
            # print(dyes())
            # rename list elements
            if (input$dyeSlct != input$dyeIdText) {
              values$rdml$dye <- values$rdml$dye
              updateSelectizeInput(session,
                                   "dyeSlct",
                                   choices = names(values$rdml$dye),
                                   selected = input$dyeIdText)
              updateSelectInput(session,
                                "targetDyeIdSlct",
                                choices = names(values$rdml$dye))
            }
          })
        },
        error = function(e) {
          print(e$message)
          dyes
          }
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
        updateSelectInput(session,
                          "targetDyeIdSlct",
                          choices = names(values$rdml$dye))
      })
    })
  
  # Sample Tab -----------------------------------------------------  
  
  
  # init
  observe({
    if (is.null(values$rdml$sample))
      return(NULL)
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
      } else {
        updateTextInput(session,
                        "sampleIdText",
                        value = input$sampleSlct)
      }
    })
  })
  
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
        idType$new(testEmptyInput(input$sampleIdText)),
        testEmptyInput(input$sampleDescriptionText),
        lapply(input$sampleDocumentationSlct,
               function(doc) idReferencesType$new(doc)),
        xRef,
        annotation,
        
        sampleTypeType$new(input$sampleTypeSlct),
        testEmptyInput(input$sampleInterRunCalibratorChk),
        
        tryCatch({
          quantityType$new(as.numeric(input$sampleQuantityValueText),
                           quantityUnitType$new(input$sampleQuantityUnitText))
        },
        error = function(e) {
          print(e$message)
          NULL}
        ),
        
        testEmptyInput(input$sampleCalibrationSampleChk),
        # NULL,
        tryCatch({
          cdnaSynthesisMethodType$new(
            testEmptyInput(input$sampleCsmEnzymeText),
            primingMethodType$new(input$sampleCsmPrimingMethodSlct),
            testEmptyInput(input$sampleCsmDnaseTreatmentChk),
            {
              if (input$sampleCsmTccSlct == "")
                NULL
              else
                idReferencesType$new(
                  testEmptyInput(input$sampleCsmTccSlct))
            })
        },
        error = function(e) {
          print(e$message)
          NULL}
        ),
        
        tryCatch({
          templateQuantityType$new(
            testEmptyInput(as.numeric(input$sampleTemplateQuantityConcText)),
            nucleotideType$new(testEmptyInput(input$sampleTemplateQuantityNucleotideSlct)))
        },
        error = function(e) {
          print(e$message)
          NULL}
        )
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
      })
    },
    error = function(e) print(e$message)
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
    error = function(e) print(e$message)
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
    error = function(e) print(e$message)
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
          "targetxRefnSlct",
          choices = names(values$rdml$target[[input$targetSlct]]$annotation))
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
               function(doc) idReferencesType$new(doc)),
        xRef,
        
        targetTypeType$new(input$targetTypeSlct),
        testEmptyInput(input$targetAemText),
        testEmptyInput(as.numeric(input$targetAeText)),
        testEmptyInput(as.numeric(input$targetAeSeText)),
        testEmptyInput(as.numeric(input$targetDetectionLimitText)),
        
        dyeId= idReferencesType$new(testEmptyInput(input$targetDyeIdSlct)),
        
        {
          isolate({
            seq <- values$rdml$target[[input$targetSlct]]$sequences
          })
          seq[[input$targetSequencesTypeSlct]] <- 
            tryCatch({
              oligoType$new(
                input$targetSequences3PrimeTagText,
                input$targetSequences5PrimeTagText,
                input$targetSequencesSequenceText
              )
            },
            error = function(e) {
              print(e$message)
              NULL}
            )
          seq
        },
        
        tryCatch({
          commercialAssayType$new(
            input$targetCaCompanyText,
            input$targetCaOrderNumberText
          )
        },
        error = function(e) {
          print(e$message)
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
      })
    },
    error = function(e) print(paste("target:", e$message))
    )
  })
  
  # remove target
  observe({
    input$removetargetBtn
    isolate({
      values$rdml$target[[input$targetSlct]] <- NULL
      updateSelectizeInput(session,
                           "targetSlct",
                           choices = names(values$rdml$target))
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
    error = function(e) print(e$message)
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






