"%>%" <- function(...) "%>>%"(...)

with.names <- function(l, id) {
  if (is.null(l))
    return(NULL)
  id <- substitute(id) %>% as.character
  n <- unname(sapply(l, function(el) el[[id]]))
  if (is.null(n))
    return(l)
  names(l) <- n
  l
}

# rdmlBaseType ------------------------------------------------------------
rdmlBaseType <-
  R6Class("rdmlBaseType",
          # class = FALSE,
          public = list(
            clone = function() {
              content <- 
                llply(names(private),
                      function(name) {
                        if (private[[name]] %>% is.null)
                          return(NULL)
                        if (private[[name]] %>%
                            class %>% 
                            tail(1) != "R6") {
                          if (is.list(private[[name]]))
                            llply(private[[name]],
                                  function(el) el$clone())
                          else
                            private[[name]]
                        } else {
                          private[[name]]$clone()
                        }
                      })
              names(content) <- gsub(".(.*)", "\\1", names(private))
              do.call(
                what = eval(parse(text = sprintf("%s$new", class(self)[1]))),
                args = content)
            } #,
            #             print = function() {
            #               sapply(names(private), function(n) private[[n]])
            #             }
          ))

# rdmlIdType ------------------------------------------------------------
rdmlIdType <- 
  R6Class("rdmlIdType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(publisher,
                                  serialNumber,
                                  MD5Hash = NULL) {
              assert_that(is.string(publisher))
              assert_that(is.string(serialNumber))
              assert_that(is.opt.string(MD5Hash))
              private$.publisher <- publisher
              private$.serialNumber <- serialNumber
              private$.MD5Hash <- MD5Hash
            }
          ),
          private = list(
            .publisher = NULL,
            .serialNumber = NULL,
            .MD5Hash = NULL
          ),
          active = list(
            publisher = function(publisher) {
              if (missing(publisher))
                return(private$.publisher)
              assert_that(is.string(publisher))
              private$.publisher <- publisher
            },
            serialNumber = function(serialNumber) {
              if (missing(serialNumber))
                return(private$.serialNumber)
              assert_that(is.string(serialNumber))
              private$.serialNumber <- serialNumber
            },
            MD5Hash = function(MD5Hash) {
              if (missing(MD5Hash))
                return(private$.MD5Hash)
              assert_that(is.opt.string(MD5Hash))
              private$.MD5Hash <- MD5Hash
            }
          ))

# experimenterType ------------------------------------------------------------
experimenterType <- 
  R6Class("experimenterType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  firstName,
                                  lastName,
                                  email = NULL,
                                  labName = NULL,
                                  labAddress = NULL) {
              assert_that(is.string(id))
              assert_that(is.string(firstName))
              assert_that(is.string(lastName))
              assert_that(is.opt.string(email))
              assert_that(is.opt.string(labName))
              assert_that(is.opt.string(labAddress))
              private$.id <- id
              private$.firstName <- firstName
              private$.lastName <- lastName
              private$.email <- email
              private$.labName <- labName
              private$.labAddress <- labAddress
            }
          ),
          private = list(
            .id = NULL,
            .firstName = NULL,
            .lastName = NULL,
            .email = NULL,
            .labName = NULL,
            .labAddress = NULL
          ),
          active = list(
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assert_that(is.string(id))
              private$.id <- id
            },
            firstName = function(firstName) {
              if (missing(firstName))
                return(private$.firstName)
              assert_that(is.string(firstName))
              private$.firstName <- firstName
            },
            lastName = function(lastName) {
              if (missing(lastName))
                return(private$.lastName)
              assert_that(is.string(lastName))
              private$.lastName <- lastName
            },
            email = function(email) {
              if (missing(email))
                return(private$.email)
              assert_that(is.opt.string(email))
              private$.email <- email
            },
            labName = function(labName) {
              if (missing(labName))
                return(private$.labName)
              assert_that(is.opt.string(labName))
              private$.labName <- labName
            },
            labAddress = function(labAddress) {
              if (missing(labAddress))
                return(private$.labAddress)
              assert_that(is.opt.string(labAddress))
              private$.labAddress <- labAddress
            }
          ))

# documentationType ------------------------------------------------------------
documentationType <- 
  R6Class("documentationType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  text = NULL) {
              assert_that(is.string(id))
              assert_that(is.opt.string(text))
              private$.id <- id
              private$.text <- text
            }
          ),
          private = list(
            .id = NULL,
            .text = NULL
          ),
          active = list(
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assert_that(is.string(id))
              private$.id <- id
            },
            text = function(text) {
              if (missing(text))
                return(private$.text)
              assert_that(is.opt.string(text))
              private$.text <- text
            }
          ))

# dyeType ------------------------------------------------------------
dyeType <- 
  R6Class("dyeType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  description = NULL) {
              assert_that(is.string(id))
              assert_that(is.opt.string(description))
              private$.id <- id
              private$.description <- description
            }
          ),
          private = list(
            .id = NULL,
            .description = NULL
          ),
          active = list(
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assert_that(is.string(id))
              private$.id <- id
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert_that(is.opt.string(description))
              private$.description <- description
            }
          ))

# idReferenceType ------------------------------------------------------------
idReferenceType <- 
  R6Class("idReferenceType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id) {
              assert_that(is.string(id))
              private$.id <- id
            }
          ),
          private = list(
            .id = NULL
          ),
          active = list(
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assert_that(is.string(id))
              private$.id <- id
            }
          ))

# xRefType ------------------------------------------------------------
xRefType <- 
  R6Class("xRefType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(name = NULL,
                                  id = NULL) {
              assert_that(is.opt.string(name))
              assert_that(is.opt.string(id))
              private$.name <- name
              private$.id <- id
            }
          ),
          private = list(
            .name = NULL,
            .id = NULL
          ),
          active = list(
            name = function(name) {
              if (missing(name))
                return(private$.name)
              assert_that(is.opt.string(name))
              private$.name <- name
            },
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assert_that(is.opt.string(id))
              private$.id <- id
            }
          ))

# annotationType ------------------------------------------------------------
annotationType <- 
  R6Class("annotationType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(property,
                                  value) {
              assert_that(is.string(property))
              assert_that(is.string(value))
              private$.property <- property
              private$.value <- value
            }
          ),
          private = list(
            .property = NULL,
            .value = NULL
          ),
          active = list(
            property = function(property) {
              if (missing(property))
                return(private$.property)
              assert_that(is.string(property))
              private$.property <- property
            },
            value = function(value) {
              if (missing(value))
                return(private$.value)
              assert_that(is.string(value))
              private$.value <- value
            }
          ))

# quantityType ------------------------------------------------------------
quantityType <- 
  R6Class("quantityType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(value,
                                  unit) {
              assert_that(is.float(value))
              assert_that(is.string(unit))
              private$.value <- value
              private$.unit <- unit
            }
          ),
          private = list(
            .value = NULL,
            .unit = NULL
          ),
          active = list(
            value = function(value) {
              if (missing(value))
                return(private$.value)
              assert_that(is.float(value))
              private$.value <- value
            },
            unit = function(unit) {
              if (missing(unit))
                return(private$.unit)
              assert_that(is.string(unit))
              private$.unit <- unit
            }
          ))


# cdnaSynthesisMethodType ------------------------------------------------------------
cdnaSynthesisMethodType <- 
  R6Class("cdnaSynthesisMethodType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(enzyme = NULL,
                                  primingMethod = NULL,
                                  dnaseTreatment = NULL,
                                  thermalCyclingConditions = NULL) {
              assert_that(is.opt.string(enzyme))
              assert_that(is.opt.string(primingMethod))
              assert_that(is.opt.logical(dnaseTreatment))
              assert_that(is.opt.type(thermalCyclingConditions,
                                      idReferenceType))
              private$.enzyme <- enzyme
              private$.primingMethod <- primingMethod
              private$.dnaseTreatment <- dnaseTreatment
              private$.thermalCyclingConditions <- thermalCyclingConditions
            }
          ),
          private = list(
            .enzyme = NULL,
            .primingMethod = NULL,
            .dnaseTreatment = NULL,
            .thermalCyclingConditions = NULL
          ),
          active = list(
            enzyme = function(enzyme) {
              if (missing(enzyme))
                return(private$.enzyme)
              assert_that(is.opt.string(enzyme))
              private$.enzyme <- enzyme
            },
            primingMethod = function(primingMethod) {
              if (missing(primingMethod))
                return(private$.primingMethod)
              assert_that(is.opt.string(primingMethod))
              private$.primingMethod <- primingMethod
            },
            dnaseTreatment = function(dnaseTreatment) {
              if (missing(dnaseTreatment))
                return(private$.dnaseTreatment)
              assert_that(is.opt.logical(dnaseTreatment))
              private$.dnaseTreatment <- dnaseTreatment
            },
            thermalCyclingConditions = function(thermalCyclingConditions) {
              if (missing(thermalCyclingConditions))
                return(private$.thermalCyclingConditions)
              assert_that(is.opt.type(thermalCyclingConditions,
                                      idReferenceType))
              private$.thermalCyclingConditions <- thermalCyclingConditions
            }
          ))

# templateQuantityType ------------------------------------------------------------
templateQuantityType <- 
  R6Class("templateQuantityType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(conc,
                                  nucleotide) {
              assert_that(is.float(conc))
              assert_that(is.string(nucleotide))
              private$.conc <- conc
              private$.nucleotide <- nucleotide
            }
          ),
          private = list(
            .conc = NULL,
            .nucleotide = NULL
          ),
          active = list(
            conc = function(conc) {
              if (missing(conc))
                return(private$.conc)
              assert_that(is.float(conc))
              private$.conc <- conc
            },
            nucleotide = function(nucleotide) {
              if (missing(nucleotide))
                return(private$.nucleotide)
              assert_that(is.string(nucleotide))
              private$.nucleotide <- nucleotide
            }
          ))

# sampleType ------------------------------------------------------------
sampleType <- 
  R6Class("sampleType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  description = NULL,
                                  documentation = NULL,
                                  xRef = NULL,
                                  annotation = NULL,
                                  type = "unkn",
                                  interRunCalibrator = FALSE,
                                  quantity = NULL,
                                  calibratorSample = FALSE,
                                  cdnaSynthesisMethod = NULL,
                                  templateQuantity = NULL) {
              assert_that(is.string(id))
              assert_that(is.opt.string(description))
              assert_that(is.opt.list.type(documentation,
                                           idReferenceType))
              assert_that(is.opt.list.type(xRef,
                                           xRefType))
              assert_that(is.opt.list.type(annotation,
                                           annotationType))
              assert_that(is.string(type))
              assert_that(is.opt.logical(interRunCalibrator))
              assert_that(is.opt.type(quantity,
                                      quantityType))
              assert_that(is.opt.logical(calibratorSample))
              assert_that(is.opt.type(cdnaSynthesisMethod,
                                      cdnaSynthesisMethodType))
              assert_that(is.opt.type(templateQuantity,
                                      templateQuantityType))
              
              private$.id <- id
              private$.description <- description
              private$.documentation <- with.names(documentation, id)
              private$.xRef <- with.names(xRef, name)
              private$.annotation <- with.names(annotation, property)
              private$.type <- type
              private$.interRunCalibrator <- interRunCalibrator
              private$.quantity <- quantity
              private$.calibratorSample <- calibratorSample
              private$.cdnaSynthesisMethod <- cdnaSynthesisMethod
              private$.templateQuantity <- templateQuantity
            }
          ),
          private = list(
            .id = NULL,
            .description = NULL,
            .documentation = NULL,
            .xRef = NULL,
            .annotation = NULL,
            .type = NULL,
            .interRunCalibrator = NULL,
            .quantity = NULL,
            .calibratorSample = NULL,
            .cdnaSynthesisMethod = NULL,
            .templateQuantity = NULL
          ),
          active = list(
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assert_that(is.string(id))
              private$.id <- id
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert_that(is.opt.string(description))
              private$.description <- description
            },
            documentation = function(documentation) {
              if (missing(documentation))
                return(private$.documentation)
              assert_that(is.opt.list.type(documentation,
                                           idReferenceType))
              private$.documentation <- with.names(documentation, id)
            },
            xRef = function(xRef) {
              if (missing(xRef))
                return(private$.xRef)
              assert_that(is.opt.list.type(xRef,
                                           xRefType))
              private$.xRef <- with.names(xRef, name)
            },
            annotation = function(annotation) {
              if (missing(annotation))
                return(private$.annotation)
              assert_that(is.opt.list.type(annotation,
                                           annotationType))
              private$.annotation <- with.names(annotation, property)
            },
            type = function(type) {
              if (missing(type))
                return(private$.type)
              assert_that(is.string(type))
              private$.type <- type
            },
            interRunCalibrator = function(interRunCalibrator) {
              if (missing(interRunCalibrator))
                return(private$.interRunCalibrator)
              assert_that(is.opt.logical(interRunCalibrator))
              private$.interRunCalibrator <- interRunCalibrator
            },
            quantity = function(quantity) {
              if (missing(quantity))
                return(private$.quantity)
              assert_that(is.opt.type(quantity,
                                      quantityType))
              private$.quantity <- quantity
            },
            calibratorSample = function(calibratorSample) {
              if (missing(calibratorSample))
                return(private$.calibratorSample)
              assert_that(is.opt.logical(calibratorSample))
              private$.calibratorSample <- calibratorSample
            },
            cdnaSynthesisMethod = function(cdnaSynthesisMethod) {
              if (missing(cdnaSynthesisMethod))
                return(private$.cdnaSynthesisMethod)
              assert_that(is.opt.type(cdnaSynthesisMethod,
                                      cdnaSynthesisMethodType))
              private$.cdnaSynthesisMethod <- cdnaSynthesisMethod
            },
            templateQuantity = function(templateQuantity) {
              if (missing(templateQuantity))
                return(private$.templateQuantity)
              assert_that(is.opt.type(templateQuantity,
                                      templateQuantityType))
              private$.templateQuantity <- templateQuantity
            }
          ))

# oligoType ------------------------------------------------------------
oligoType <- 
  R6Class("oligoType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(threePrimeTag = NULL,
                                  fivePrimeTag = NULL,
                                  sequence) {
              assert_that(is.opt.string(threePrimeTag))
              assert_that(is.opt.string(fivePrimeTag))
              assert_that(is.string(sequence))
              private$.threePrimeTag <- threePrimeTag
              private$.fivePrimeTag <- fivePrimeTag
              private$.sequence <- sequence
            }
          ),
          private = list(
            .threePrimeTag = NULL,
            .fivePrimeTag = NULL,
            .sequence = NULL
          ),
          active = list(
            threePrimeTag = function(threePrimeTag) {
              if (missing(threePrimeTag))
                return(private$.threePrimeTag)
              assert_that(is.opt.string(threePrimeTag))
              private$.threePrimeTag <- threePrimeTag
            },
            fivePrimeTag = function(fivePrimeTag) {
              if (missing(fivePrimeTag))
                return(private$.fivePrimeTag)
              assert_that(is.opt.string(fivePrimeTag))
              private$.fivePrimeTag <- fivePrimeTag
            },
            sequence = function(sequence) {
              if (missing(sequence))
                return(private$.sequence)
              assert_that(is.string(sequence))
              private$.sequence <- sequence
            }
          ))


# sequencesType ------------------------------------------------------------
sequencesType <- 
  R6Class("sequencesType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(forwardPrimer = NULL,
                                  reversePrimer = NULL,
                                  probe1 = NULL,
                                  probe2 = NULL,
                                  amplicon = NULL) {
              assert_that(is.opt.type(forwardPrimer,
                                      oligoType))
              assert_that(is.opt.type(reversePrimer,
                                      oligoType))
              assert_that(is.opt.type(probe1,
                                      oligoType))
              assert_that(is.opt.type(probe2,
                                      oligoType))
              assert_that(is.opt.type(amplicon,
                                      oligoType))
              private$.forwardPrimer <- forwardPrimer
              private$.reversePrimer <- reversePrimer
              private$.probe1 <- probe1
              private$.probe2 <- probe2
              private$.amplicon <- amplicon
            }
          ),
          private = list(
            .forwardPrimer = NULL,
            .reversePrimer = NULL,
            .probe1 = NULL,
            .probe2 = NULL,
            .amplicon = NULL
          ),
          active = list(
            forwardPrimer = function(forwardPrimer) {
              if (missing(forwardPrimer))
                return(private$.forwardPrimer)
              assert_that(is.opt.type(forwardPrimer,
                                      oligoType))
              private$.forwardPrimer <- forwardPrimer
            },
            reversePrimer = function(reversePrimer) {
              if (missing(reversePrimer))
                return(private$.reversePrimer)
              assert_that(is.opt.type(reversePrimer,
                                      oligoType))
              private$.reversePrimer <- reversePrimer
            },
            probe1 = function(probe1) {
              if (missing(probe1))
                return(private$.probe1)
              assert_that(is.opt.type(probe1,
                                      oligoType))
              private$.probe1 <- probe1
            },
            probe2 = function(probe2) {
              if (missing(probe2))
                return(private$.probe2)
              assert_that(is.opt.type(probe2,
                                      oligoType))
              private$.probe2 <- probe2
            },
            amplicon = function(amplicon) {
              if (missing(amplicon))
                return(private$.amplicon)
              assert_that(is.opt.type(amplicon,
                                      oligoType))
              private$.amplicon <- amplicon
            }
          ))

# commercialAssayType ------------------------------------------------------------
commercialAssayType <- 
  R6Class("commercialAssayType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(company,
                                  orderNumber) {
              assert_that(is.string(company))
              assert_that(is.string(orderNumber))
              private$.company <- company
              private$.orderNumber <- orderNumber
            }
          ),
          private = list(
            .company = NULL,
            .orderNumber = NULL
          ),
          active = list(
            company = function(company) {
              if (missing(company))
                return(private$.company)
              assert_that(is.string(company))
              private$.company <- company
            },
            orderNumber = function(orderNumber) {
              if (missing(orderNumber))
                return(private$.orderNumber)
              assert_that(is.string(orderNumber))
              private$.orderNumber <- orderNumber
            }
          ))

# targetType ------------------------------------------------------------
targetType <- 
  R6Class("targetType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  description = NULL,
                                  documentation = NULL,
                                  xRef = NULL,
                                  type,
                                  amplificationEfficiencyMethod = NULL,
                                  amplificationEfficiency = NULL,
                                  amplificationEfficiencySE = NULL,
                                  detectionLimit = NULL,
                                  dyeId,
                                  sequences = NULL,
                                  commercialAssay = NULL) {
              assert_that(is.string(id))
              assert_that(is.opt.string(description))
              assert_that(is.opt.list.type(documentation,
                                           idReferenceType))
              assert_that(is.opt.list.type(xRef,
                                           xRefType))
              assert_that(is.string(type))
              assert_that(is.opt.string(amplificationEfficiencyMethod))
              assert_that(is.opt.double(amplificationEfficiency))
              assert_that(is.opt.double(amplificationEfficiencySE))
              assert_that(is.opt.double(detectionLimit))
              assert_that(is.type(dyeId,
                                  idReferenceType))
              assert_that(is.opt.list.type(sequences,
                                           sequencesType))
              assert_that(is.opt.list.type(commercialAssay,
                                           commercialAssayType))
              
              private$.id <- id
              private$.description <- description
              private$.documentation <- with.names(documentation, id)
              private$.xRef <- with.names(xRef, name)
              private$.type <- type
              private$.amplificationEfficiencyMethod <- amplificationEfficiencyMethod
              private$.amplificationEfficiency <- amplificationEfficiency
              private$.amplificationEfficiencySE <- amplificationEfficiencySE
              private$.detectionLimit <- detectionLimit
              private$.dyeId <- dyeId
              private$.sequences <- sequences
              private$.commercialAssay <- commercialAssay
            }
          ),
          private = list(
            .id = NULL,
            .description = NULL,
            .documentation = NULL,
            .xRef = NULL,
            .type = NULL,
            .amplificationEfficiencyMethod = NULL,
            .amplificationEfficiency = NULL,
            .amplificationEfficiencySE = NULL,
            .detectionLimit = NULL,
            .dyeId = NULL,
            .sequences = NULL,
            .commercialAssay = NULL
          ),
          active = list(
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assert_that(is.string(id))
              private$.id <- id
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert_that(is.opt.string(description))
              private$.description <- description
            },
            documentation = function(documentation) {
              if (missing(documentation))
                return(private$.documentation)
              assert_that(is.opt.list.type(documentation,
                                           idReferenceType))
              private$.documentation <- with.names(documentation, id)
            },
            xRef = function(xRef) {
              if (missing(xRef))
                return(private$.xRef)
              assert_that(is.opt.list.type(xRef,
                                           xRefType))
              private$.xRef <- with.names(xRef, name)
            },
            type = function(type) {
              if (missing(type))
                return(private$.type)
              assert_that(is.string(type))
              private$.type <- type
            },
            amplificationEfficiencyMethod = 
              function(amplificationEfficiencyMethod) {
                if (missing(amplificationEfficiencyMethod))
                  return(private$.amplificationEfficiencyMethod)
                assert_that(is.opt.string(amplificationEfficiencyMethod))
                private$.amplificationEfficiencyMethod <- amplificationEfficiencyMethod
              },
            amplificationEfficiency = function(amplificationEfficiency) {
              if (missing(amplificationEfficiency))
                return(private$.amplificationEfficiency)
              assert_that(is.opt.double(amplificationEfficiency))
              private$.amplificationEfficiency <- amplificationEfficiency
            },
            amplificationEfficiencySE = function(amplificationEfficiencySE) {
              if (missing(amplificationEfficiencySE))
                return(private$.amplificationEfficiencySE)
              assert_that(is.opt.double(amplificationEfficiencySE))
              private$.amplificationEfficiencySE <- amplificationEfficiencySE
            },
            detectionLimit = function(detectionLimit) {
              if (missing(detectionLimit))
                return(private$.detectionLimit)
              assert_that(is.opt.double(detectionLimit))
              private$.detectionLimit <- detectionLimit
            },
            dyeId = function(dyeId) {
              if (missing(dyeId))
                return(private$.dyeId)
              assert_that(is.type(dyeId,
                                  idReferenceType))
              private$.dyeId <- dyeId
            },
            sequences = function(sequences) {
              if (missing(sequences))
                return(private$.sequences)
              assert_that(is.opt.list.type(sequences,
                                           sequencesType))
              private$.sequences <- sequences
            },
            commercialAssay = function(commercialAssay) {
              if (missing(commercialAssay))
                return(private$.commercialAssay)
              assert_that(is.opt.list.type(commercialAssay,
                                           commercialAssayType))
              private$.commercialAssay <- commercialAssay
            }
          ))
