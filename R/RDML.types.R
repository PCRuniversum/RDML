# "%>%" <- function(...) "%>>%"(...)

with.names <- function(l, id) {
  if (is.null(l))
    return(NULL)
  n <- list.select(l,
                   eval(id)) %>% 
    unlist
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
            },
            print = function(...) {
              cat("Publisher\t: ",
                  private$.publisher,
                  "\nS/N\t\t: ",
                  private$.serialNumber)
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

# idType ------------------------------------------------------------
idType <- 
  R6Class("idType",
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

# idReferencesType ------------------------------------------------------------
idReferencesType <- 
  R6Class("idReferencesType",
          # class = FALSE,
          inherit = idType)

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
              assert_that(is.type(id, idType))
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
              assert_that(is.type(id, idType))
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
              assert_that(is.type(id, idType))
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
              assert_that(is.type(id, idType))
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
              assert_that(is.type(id, idType))
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
              assert_that(is.type(id, idType))
              private$.id <- id
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert_that(is.opt.string(description))
              private$.description <- description
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
              assert_that(is.type(unit,
                                  quantityUnitType))
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
              assert_that(is.type(unit,
                                  quantityUnitType))
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
              assert_that(is.opt.type(primingMethod,
                                      primingMethodType))
              assert_that(is.opt.flag(dnaseTreatment))
              assert_that(is.opt.type(thermalCyclingConditions,
                                      idType))
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
              assert_that(is.opt.type(primingMethod,
                                      primingMethodType))
              private$.primingMethod <- primingMethod
            },
            dnaseTreatment = function(dnaseTreatment) {
              if (missing(dnaseTreatment))
                return(private$.dnaseTreatment)
              assert_that(is.opt.flag(dnaseTreatment))
              private$.dnaseTreatment <- dnaseTreatment
            },
            thermalCyclingConditions = function(thermalCyclingConditions) {
              if (missing(thermalCyclingConditions))
                return(private$.thermalCyclingConditions)
              assert_that(is.opt.type(thermalCyclingConditions,
                                      idType))
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
              assert_that(is.type(nucleotide,
                                  nucleotideType))
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
              assert_that(is.type(nucleotide,
                                  nucleotideType))
              private$.nucleotide <- nucleotide
            }
          ))


# enumType ------------------------------------------------------------
enumType <- 
  R6Class("enumType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(value, ...) {
              assert_that(is.enum(value, private$.levels))
              private$.value <- value
            }
          ),
          private = list(
            .value = NULL
          ),
          active = list(
            value = function(value) {
              if (missing(value))
                return(private$.value)
              assert_that(is.enum(value, private$.levels))
              private$.value <- value
            }
          ))

# sampleTypeType ------------------------------------------------------------
sampleTypeType <- 
  R6Class("sampleTypeType",
          # class = FALSE,
          inherit = enumType,
          private = list(
            .levels = c("unkn", "ntc", "nac",
                        "std", "ntp", "nrt",
                        "pos", "opt")
          )
  )

# quantityUnitType ------------------------------------------------------------
quantityUnitType <- 
  R6Class("quantityUnitType",
          # class = FALSE,
          inherit = enumType,
          private = list(
            .levels = c("cop", "fold", "dil", "ng", "nMol", "other")
          )
  )

# primingMethodType ------------------------------------------------------------
primingMethodType <- 
  R6Class("primingMethodType",
          # class = FALSE,
          inherit = enumType,
          private = list(
            .levels = c("oligo-dt",
                        "random",
                        "target-specific",
                        "oligo-dt and random",
                        "other")
          )
  )

# nucleotideType ------------------------------------------------------------
nucleotideType <- 
  R6Class("nucleotideType",
          # class = FALSE,
          inherit = enumType,
          private = list(
            .levels = c("DNA",
                        "genomic-DNA",
                        "cDNA",
                        "RNA")
          )
  )

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
                                  type = sampleTypeType$new("unkn"),
                                  interRunCalibrator = FALSE,
                                  quantity = NULL,
                                  calibratorSample = FALSE,
                                  cdnaSynthesisMethod = NULL,
                                  templateQuantity = NULL) {
              assert_that(is.type(id, idType))
              assert_that(is.opt.string(description))
              assert_that(is.opt.list.type(documentation,
                                           idReferencesType))
              assert_that(is.opt.list.type(xRef,
                                           xRefType))
              assert_that(is.opt.list.type(annotation,
                                           annotationType))
              assert_that(is.type(type,
                                  sampleTypeType))
              assert_that(is.opt.flag(interRunCalibrator))
              assert_that(is.opt.type(quantity,
                                      quantityType))
              assert_that(is.opt.flag(calibratorSample))
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
              assert_that(is.type(id, idType))
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
                                           idReferencesType))
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
              assert_that(is.type(type,
                                  sampleTypeType))
              private$.type <- type
            },
            interRunCalibrator = function(interRunCalibrator) {
              if (missing(interRunCalibrator))
                return(private$.interRunCalibrator)
              assert_that(is.opt.flag(interRunCalibrator))
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
              assert_that(is.opt.flag(calibratorSample))
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

# targetTypeType ------------------------------------------------------------
targetTypeType <- 
  R6Class("targetTypeType",
          # class = FALSE,
          inherit = enumType,
          private = list(
            .levels = c("ref",
                        "toi")
          )
  )


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
              assert_that(is.type(id, idType))
              assert_that(is.opt.string(description))
              assert_that(is.opt.list.type(documentation,
                                           idReferencesType))
              assert_that(is.opt.list.type(xRef,
                                           xRefType))
              assert_that(is.type(type,
                                  targetTypeType))
              assert_that(is.opt.string(amplificationEfficiencyMethod))
              assert_that(is.opt.double(amplificationEfficiency))
              assert_that(is.opt.double(amplificationEfficiencySE))
              assert_that(is.opt.double(detectionLimit))
              assert_that(is.type(dyeId,
                                  idReferencesType))
              assert_that(is.opt.type(sequences,
                                      sequencesType))
              assert_that(is.opt.type(commercialAssay,
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
              assert_that(is.type(id, idType))
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
                                           idReferencesType))
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
              assert_that(is.type(type,
                                  targetTypeType))
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
                                  idReferencesType))
              private$.dyeId <- dyeId
            },
            sequences = function(sequences) {
              if (missing(sequences))
                return(private$.sequences)
              assert_that(is.opt.type(sequences,
                                      sequencesType))
              private$.sequences <- sequences
            },
            commercialAssay = function(commercialAssay) {
              if (missing(commercialAssay))
                return(private$.commercialAssay)
              assert_that(is.opt.type(commercialAssay,
                                      commercialAssayType))
              private$.commercialAssay <- commercialAssay
            }
          ))

# dpAmpCurveType ------------------------------------------------------------
dpAmpCurveType <- 
  R6Class("dpAmpCurveType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(cyc,
                                  tmp = NULL,
                                  fluor) {
              assert_that(is.float(cyc))
              assert_that(is.opt.double(tmp))
              assert_that(is.float(fluor))
              private$.cyc <- cyc
              private$.tmp <- tmp
              private$.fluor <- fluor
            },
            AsVector = function() {
              c(cyc = private$.cyc,
                {
                  if (!is.null(private$.tmp))
                    c(tmp = private$.tmp)
                  },
                fluor = private$.fluor)
            }
          ),
          private = list(
            .cyc = NULL,
            .tmp = NULL,
            .fluor = NULL
          ),
          active = list(
            cyc = function(cyc) {
              if (missing(cyc))
                return(private$.cyc)
              assert_that(is.float(cyc))
              private$.cyc <- cyc
            },
            tmp = function(tmp) {
              if (missing(tmp))
                return(private$.tmp)
              assert_that(is.opt.double(tmp))
              private$.tmp <- tmp
            },
            fluor = function(fluor) {
              if (missing(fluor))
                return(private$.fluor)
              assert_that(is.float(fluor))
              private$.fluor <- fluor
            }
          ))

# dpMeltingCurveType ------------------------------------------------------------
dpMeltingCurveType <- 
  R6Class("dpMeltingCurveType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(tmp,
                                  fluor) {
              assert_that(is.float(tmp))
              assert_that(is.float(fluor))
              private$.tmp <- tmp
              private$.fluor <- fluor
            },
            AsVector = function() {
              c(cyc = private$.tmp,
                fluor = private$.fluor)
            }
          ),
          private = list(
            .cyc = NULL,
            .tmp = NULL,
            .fluor = NULL
          ),
          active = list(
            tmp = function(tmp) {
              if (missing(tmp))
                return(private$.tmp)
              assert_that(is.float(tmp))
              private$.tmp <- tmp
            },
            fluor = function(fluor) {
              if (missing(fluor))
                return(private$.fluor)
              assert_that(is.float(fluor))
              private$.fluor <- fluor
            }
          ))

# dataType ------------------------------------------------------------
dataType <- 
  R6Class("dataType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(tar,
                                  cq = NULL,
                                  excl = NULL,
                                  adp = NULL,
                                  mdp = NULL,
                                  endPt = NULL,
                                  bgFluor = NULL,
                                  bgFluorSlp = NULL,
                                  quantFluor = NULL) {
              assert_that(is.type(tar,
                                  idReferencesType))
              assert_that(is.opt.double(cq))
              assert_that(is.opt.string(excl))
              assert_that(is.opt.list.type(adp,
                                           dpAmpCurveType))
              assert_that(is.opt.list.type(mdp,
                                           dpMeltingCurveType))
              assert_that(is.opt.double(endPt))
              assert_that(is.opt.double(bgFluor))
              assert_that(is.opt.double(bgFluorSlp))
              assert_that(is.opt.double(quantFluor))
              
              private$.tar <- tar
              private$.cq <- cq
              private$.excl <- excl
              private$.adp <- adp
              private$.mdp <- mdp
              private$.endPt <- endPt 
              private$.bgFluor <- bgFluor
              private$.bgFluorSlp <- bgFluorSlp
              private$.quantFluor <- quantFluor
            },
            AsDataFrame = function(dp.type = "adp") {
              assert_that(is.string(dp.type))
              ldply(private[[paste0(".", dp.type)]],
                    function(dp) dp$AsVector())
            }
          ),
          private = list(
            .tar = NULL,
            .cq = NULL,
            .excl = NULL,
            .adp = NULL,
            .mdp = NULL,
            .endPt = NULL,
            .bgFluor = NULL,
            .bgFluorSlp = NULL,
            .quantFluor = NULL
          ),
          active = list(
            tar = function(tar) {
              if (missing(tar))
                return(private$.tar)
              assert_that(is.type(tar,
                                  idReferencesType))
              private$.tar <- tar
            },
            cq = function(cq) {
              if (missing(cq))
                return(private$.cq)
              assert_that(is.opt.double(cq))
              private$.cq <- cq
            },
            excl = function(excl) {
              if (missing(excl))
                return(private$.excl)
              assert_that(is.opt.string(excl))
              private$.excl <- excl
            },
            adp = function(adp) {
              if (missing(adp))
                return(private$.adp)
              assert_that(is.opt.list.type(adp,
                                           dpAmpCurveType))
              private$.adp <- adp
            },
            mdp = function(mdp) {
              if (missing(mdp))
                return(private$.mdp)
              assert_that(is.opt.list.type(mdp,
                                           dpMeltingCurveType))
              private$.mdp <- mdp
            },
            endPt = function(endPt) {
              if (missing(endPt))
                return(private$.endPt)
              assert_that(is.opt.double(endPt))
              private$.endPt <- endPt
            },
            bgFluor = function(bgFluor) {
              if (missing(bgFluor))
                return(private$.bgFluor)
              assert_that(is.opt.double(bgFluor))
              private$.bgFluor <- bgFluor
            },
            bgFluorSlp = function(bgFluorSlp) {
              if (missing(bgFluorSlp))
                return(private$.bgFluorSlp)
              assert_that(is.opt.double(bgFluorSlp))
              private$.bgFluorSlp <- bgFluorSlp
            },
            quantFluor = function(quantFluor) {
              if (missing(quantFluor))
                return(private$.quantFluor)
              assert_that(is.opt.double(quantFluor))
              private$.quantFluor <- quantFluor
            }
          ))

# reactType ------------------------------------------------------------
reactType <- 
  R6Class("reactType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  sample,
                                  data) {
              assert_that(is.count(id))
              assert_that(is.type(sample,
                                  idReferencesType))
              assert_that(is.list.type(data,
                                       dataType))
              private$.id <- id
              private$.sample <- sample
              private$.data <- data
            },
            AsDataFrame = function(dp.type = "adp",
                                   long.table = FALSE) {
              assert_that(is.string(dp.type))
              out <- pipeline({
                private$.data
                ldply(function(data)
                  pipeline({
                    data$AsDataFrame()
                    cbind(.,
                          tar = rep(data$tar$id, ncol(.)))
                  }))
              })
              if (long.table == FALSE) out <- out %>% tidyr::spread(tar,
                                                                    fluor) 
              out
            }
          ),
          private = list(
            .id = NULL,
            .sample = NULL,
            .data = NULL
          ),
          active = list(
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assert_that(is.count(id))
              private$.id <- id
            },
            sample = function(sample) {
              if (missing(sample))
                return(private$.sample)
              assert_that(is.type(sample,
                                  idReferencesType))
              private$.sample <- sample
            },
            data = function(data) {
              if (missing(data))
                return(private$.data)
              assert_that(is.list.type(data,
                                       dataType))
              private$.data <- data
            }
          ))

# dataCollectionSoftwareType ------------------------------------------------------------
dataCollectionSoftwareType <- 
  R6Class("dataCollectionSoftwareType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(name,
                                  version) {
              assert_that(is.string(name))
              assert_that(is.string(version))
              private$.name <- name
              private$.version <- version
            }
          ),
          private = list(
            .name = NULL,
            .version = NULL
          ),
          active = list(
            name = function(name) {
              if (missing(name))
                return(private$.name)
              assert_that(is.string(name))
              private$.name <- name
            },
            version = function(version) {
              if (missing(version))
                return(private$.version)
              assert_that(is.string(version))
              private$.version <- version
            }
          ))

# cqDetectionMethodType ------------------------------------------------------------
cqDetectionMethodType <- 
  R6Class("cqDetectionMethodType",
          # class = FALSE,
          inherit = enumType,
          private = list(
            .levels = c("automated threshold and baseline settings",
                        "manual threshold and baseline settings",
                        "second derivative maximum",
                        "other")
          )
  )



# labelFormatType ------------------------------------------------------------
labelFormatType <- 
  R6Class("labelFormatType",
          # class = FALSE,
          inherit = enumType,
          private = list(
            .levels = c("ABC",
                        "123",
                        "A1a1")
          )
  )


# pcrFormatType ------------------------------------------------------------
pcrFormatType <- 
  R6Class("pcrFormatType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(rows,
                                  columns,
                                  rowLabel,
                                  columnLabel) {
              assert_that(is.count(rows))
              assert_that(is.count(columns))
              assert_that(is.type(rowLabel,
                                  labelFormatType))
              assert_that(is.type(columnLabel,
                                  labelFormatType))
              private$.rows <- rows
              private$.columns <- columns
              private$.rowLabel <- rowLabel
              private$.columnLabel <- columnLabel
            }
          ),
          private = list(
            .rows = NULL,
            .columns = NULL,
            .rowLabel = NULL,
            .columnLabel = NULL
          ),
          active = list(
            rows = function(rows) {
              if (missing(rows))
                return(private$.rows)
              assert_that(is.count(rows))
              private$.rows <- rows
            },
            columns = function(columns) {
              if (missing(columns))
                return(private$.columns)
              assert_that(is.count(columns))
              private$.columns <- columns
            },
            rowLabel = function(rowLabel) {
              if (missing(rowLabel))
                return(private$.rowLabel)
              assert_that(is.type(rowLabel,
                                  labelFormatType))
              private$.rowLabel <- rowLabel
            },
            columnLabel = function(columnLabel) {
              if (missing(columnLabel))
                return(private$.columnLabel)
              assert_that(is.type(columnLabel,
                                  labelFormatType))
              private$.columnLabel <- columnLabel
            }
          ))

# runType ------------------------------------------------------------
runType <- 
  R6Class("runType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  description = NULL,
                                  documentation = NULL,
                                  experimenter = NULL,
                                  instrument = NULL,
                                  dataCollectionSoftware = NULL,
                                  backgroundDeterminationMethod = NULL,
                                  cqDetectionMethod = NULL,
                                  thermalCyclingConditions = NULL,
                                  pcrFormat,
                                  runDate = NULL,
                                  react = NULL) {
              assert_that(is.type(id,
                                  idType))
              assert_that(is.string(description))
              assert_that(is.list.type(documentation,
                                       idReferencesType))
              assert_that(is.list.type(experimenter,
                                       idReferencesType))
              assert_that(is.string(instrument))
              assert_that(is.opt.type(dataCollectionSoftware,
                                      dataCollectionSoftwareType))
              assert_that(is.string(backgroundDeterminationMethod))
              assert_that(is.opt.type(cqDetectionMethod,
                                      cqDetectionMethodType))
              assert_that(is.opt.type(thermalCyclingConditions,
                                      idReferencesType))
              assert_that(is.type(pcrFormat,
                                  pcrFormatType))
              assert_that(is.string(runDate)) # date time
              assert_that(is.list.type(react,
                                       reactType))
              
              private$.id <- id
              private$.description <- description
              private$.documentation <- documentation
              private$.experimenter <- experimenter
              private$.instrument <- instrument
              private$.dataCollectionSoftware <- dataCollectionSoftware
              private$.backgroundDeterminationMethod <- backgroundDeterminationMethod
              private$.cqDetectionMethod <- cqDetectionMethod
              private$.thermalCyclingConditions <- thermalCyclingConditions
              private$.pcrFormat <- pcrFormat
              private$.runDate <- runDate
              private$.react <- react
            },
            AsDataFrame = function(dp.type = "adp",
                                   long.table = FALSE) {
              assert_that(is.string(dp.type))
              out <- pipeline({
                private$.react
                ldply(function(react)
                  pipeline({
                    react$AsDataFrame(long.table = TRUE)
                    cbind(.,
                          sname = rep(
                            sprintf("%s_%s",
                                    react$id$id,
                                    react$sample$id),
                            ncol(.)))
                  }))
              })
              if (long.table == FALSE) out <- out %>% tidyr::spread(tar,
                                                                    fluor) 
              out
            }
          ),
          private = list(
            .id = NULL,
            .description = NULL,
            .documentation = NULL,
            .experimenter = NULL,
            .instrument = NULL,
            .dataCollectionSoftware = NULL,
            .backgroundDeterminationMethod = NULL,
            .cqDetectionMethod = NULL,
            .thermalCyclingConditions = NULL,
            .pcrFormat = NULL,
            .runDate = NULL,
            .react = NULL
          ),
          active = list(
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assert_that(is.type(id,
                                  idType))
              private$.id <- id
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert_that(is.string(description))
              private$.description <- description
            },
            documentation = function(documentation) {
              if (missing(documentation))
                return(private$.documentation)
              assert_that(is.list.type(documentation,
                                       idReferencesType))
              private$.documentation <- documentation
            },
            experimenter = function(experimenter) {
              if (missing(experimenter))
                return(private$.experimenter)
              assert_that(is.list.type(experimenter,
                                       idReferencesType))
              private$.experimenter <- experimenter
            },
            instrument = function(instrument) {
              if (missing(instrument))
                return(private$.instrument)
              assert_that(is.string(instrument))
              private$.instrument <- instrument
            },
            dataCollectionSoftware = function(dataCollectionSoftware) {
              if (missing(dataCollectionSoftware))
                return(private$.dataCollectionSoftware)
              assert_that(is.opt.type(dataCollectionSoftware,
                                      dataCollectionSoftwareType))
              private$.dataCollectionSoftware <- dataCollectionSoftware
            },
            backgroundDeterminationMethod = function(backgroundDeterminationMethod) {
              if (missing(backgroundDeterminationMethod))
                return(private$.backgroundDeterminationMethod)
              assert_that(is.string(backgroundDeterminationMethod))
              private$.backgroundDeterminationMethod <- backgroundDeterminationMethod
            },
            cqDetectionMethod = function(cqDetectionMethod) {
              if (missing(cqDetectionMethod))
                return(private$.cqDetectionMethod)
              assert_that(is.opt.type(cqDetectionMethod,
                                      cqDetectionMethodType))
              private$.cqDetectionMethod <- cqDetectionMethod
            },
            thermalCyclingConditions = function(thermalCyclingConditions) {
              if (missing(thermalCyclingConditions))
                return(private$.thermalCyclingConditions)
              assert_that(is.opt.type(thermalCyclingConditions,
                                      idReferencesType))
              private$.thermalCyclingConditions <- thermalCyclingConditions
            },
            pcrFormat = function(pcrFormat) {
              if (missing(pcrFormat))
                return(private$.pcrFormat)
              assert_that(is.type(pcrFormat,
                                  pcrFormatType))
              private$.pcrFormat <- pcrFormat
            },
            runDate = function(runDate) {
              if (missing(runDate))
                return(private$.runDate)
              assert_that(is.string(runDate)) # date time
              private$.runDate <- runDate
            },
            react = function(react) {
              if (missing(react))
                return(private$.react)
              assert_that(is.list.type(react,
                                       reactType))
              private$.react <- react
            }
          ))

# experimentType ------------------------------------------------------------
experimentType <- 
  R6Class("experimentType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  description = NULL,
                                  documentation = NULL,
                                  run = NULL) {
              assert_that(is.type(id,
                                  idType))
              assert_that(is.opt.string(description))
              assert_that(is.opt.list.type(documentation,
                                           idReferencesType))
              assert_that(is.opt.list.type(run,
                                           runType))
              
              private$.id <- id
              private$.description <- description
              private$.documentation <- with.names(documentation, id)
              private$.run <- run
            },
            AsDataFrame = function(dp.type = "adp",
                                   long.table = FALSE) {
              assert_that(is.string(dp.type))
              out <- pipeline({
                private$.run
                ldply(function(run)
                  pipeline({
                    run$AsDataFrame(long.table = TRUE)
                    cbind(., sname = rep(run$id$id, ncol(.)))
                  }))
              })
              if (long.table == FALSE) out <- out %>% tidyr::spread(tar,
                                                                    fluor) 
              out
            }
          ),
          private = list(
            .id = NULL,
            .description = NULL,
            .documentation = NULL,
            .run = NULL
          ),
          active = list(
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assert_that(is.type(id, idType))
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
                                           idReferencesType))
              private$.documentation <- with.names(documentation, id)
            },
            run = function(run) {
              if (missing(run))
                return(private$.run)
              assert_that(is.opt.list.type(run,
                                           runType))
              private$.run <- with.names(run, name)
            }
          ))

# lidOpenType ------------------------------------------------------------
lidOpenType <- 
  R6Class("lidOpenType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function() {
            }
          ),
          private = list(
            .lidOpen = ""
          ))

# pauseType ------------------------------------------------------------
pauseType <- 
  R6Class("pauseType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(temperature) {
              assert_that(is.float(temperature))
              private$.temperature <- temperature
            }
          ),
          private = list(
            .temperature = NULL
          ),
          active = list(
            temperature = function(temperature) {
              if (missing(temperature))
                return(private$.temperature)
              assert_that(is.float(temperature))
              private$.temperature <- temperature
            }
          ))

# loopType ------------------------------------------------------------
loopType <- 
  R6Class("loopType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(goto,
                                  repeat.n) {
              assert_that(is.count(goto))
              assert_that(is.count(repeat.n))
              private$.goto <- goto
              private$.repeat <- repeat.n
            }
          ),
          private = list(
            .goto = NULL,
            .repeat = NULL
          ),
          active = list(
            goto = function(goto) {
              if (missing(goto))
                return(private$.goto)
              assert_that(is.float(goto))
              private$.goto <- goto
            },
            repeat.n = function(repeat.n) {
              if (missing(repeat.n))
                return(private$.repeat)
              assert_that(is.float(repeat.n))
              private$.repeat <- repeat.n
            }
          ))

# measureType ------------------------------------------------------------
measureType <- 
  R6Class("measureType",
          # class = FALSE,
          inherit = enumType,
          private = list(
            .levels = c("real time",
                        "meltcurve")
          )
  )

# baseTemperatureType ------------------------------------------------------------
baseTemperatureType <- 
  R6Class("baseTemperatureType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(duration,
                                  temperatureChange = NULL,
                                  durationChange = NULL,
                                  measure = NULL,
                                  ramp = NULL) {
              assert_that(is.count(duration))
              assert_that(is.opt.double(temperatureChange))
              assert_that(is.opt.count(durationChange))
              assert_that(is.opt.type(measure,
                                      measureType))
              assert_that(is.opt.double(ramp))
              
              private$.duration <- duration
              private$.temperatureChange <- temperatureChange
              private$.durationChange <- durationChange
              private$.measure <- measure
              private$.ramp <- ramp
            }
          ),
          private = list(
            .duration = NULL,
            .temperatureChange = NULL,
            .durationChange = NULL,
            .measure = NULL,
            .ramp = NULL
          ),
          active = list(
            duration = function(duration) {
              if (missing(duration))
                return(private$.duration)
              assert_that(is.count(duration))
              private$.duration <- duration
            },
            temperatureChange = function(temperatureChange) {
              if (missing(temperatureChange))
                return(private$.temperatureChange)
              assert_that(is.opt.double(temperatureChange))
              private$.temperatureChange <- temperatureChange
            },
            durationChange = function(durationChange) {
              if (missing(durationChange))
                return(private$.durationChange)
              assert_that(is.opt.count(durationChange))
              private$.durationChange <- durationChange
            },
            measure = function(measure) {
              if (missing(measure))
                return(private$.measure)
              assert_that(is.opt.type(measure,
                                      measureType))
              private$.measure <- measure
            },
            ramp = function(ramp) {
              if (missing(ramp))
                return(private$.ramp)
              assert_that(is.opt.double(ramp))
              private$.ramp <- ramp
            }
          ))

# temperatureType ------------------------------------------------------------
temperatureType <- 
  R6Class("temperatureType",
          # class = FALSE,
          inherit = baseTemperatureType,
          public = list(
            initialize = function(temperature) {
              assert_that(is.float(temperature))
              private$.temperature <- temperature
            }
          ),
          private = list(
            .temperature = NULL
          ),
          active = list(
            temperature = function(temperature) {
              if (missing(temperature))
                return(private$.temperature)
              assert_that(is.float(temperature))
              private$.temperature <- temperature
            }
          ))

# gradientType ------------------------------------------------------------
gradientType <- 
  R6Class("gradientType",
          # class = FALSE,
          inherit = baseTemperatureType,
          public = list(
            initialize = function(highTemperature,
                                  lowTemperature) {
              assert_that(is.float(highTemperature))
              assert_that(is.float(lowTemperature))
              private$.highTemperature <- highTemperature
              private$.lowTemperature <- lowTemperature
            }
          ),
          private = list(
            .highTemperature = NULL,
            .lowTemperature = NULL
          ),
          active = list(
            highTemperature = function(highTemperature) {
              if (missing(highTemperature))
                return(private$.highTemperature)
              assert_that(is.float(highTemperature))
              private$.highTemperature <- highTemperature
            },
            lowTemperature = function(lowTemperature) {
              if (missing(lowTemperature))
                return(private$.lowTemperature)
              assert_that(is.float(lowTemperature))
              private$.lowTemperature <- lowTemperature
            }
          ))


# stepType ------------------------------------------------------------
stepType <- 
  R6Class("stepType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(nr,
                                  description = NULL,
                                  temperature = NULL,
                                  gradient = NULL,
                                  loop = NULL,
                                  pause = NULL,
                                  lidOpen = NULL) {
              assert_that(is.count(nr))
              assert_that(is.opt.string(description))
              assert_that(is.opt.type(temperature,
                                      temperatureType))
              assert_that(is.opt.type(gradient,
                                      gradientType))
              assert_that(is.opt.type(loop,
                                      loopType))
              assert_that(is.opt.type(pause,
                                      pauseType))
              assert_that(is.opt.type(lidOpen,
                                      lidOpenType))
              
              private$.nr <- nr
              private$.description <- description
              private$.temperature <- temperature
              private$.gradient <- gradient
              private$.loop <- loop
              private$.pause <- pause
              private$.lidOpen <- lidOpen
            }
          ),
          private = list(
            .nr = NULL,
            .description = NULL,
            .temperature = NULL,
            .gradient = NULL,
            .loop = NULL,
            .pause = NULL,
            .lidOpen = NULL
          ),
          active = list(
            nr = function(nr) {
              if (missing(nr))
                return(private$.nr)
              assert_that(is.count(nr))
              private$.nr <- nr
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert_that(is.opt.string(description))
              private$.description <- description
            },
            temperature = function(temperature) {
              if (missing(temperature))
                return(private$.temperature)
              assert_that(is.opt.type(temperature,
                                      temperatureType))
              private$.temperature <- temperature
            },
            gradient = function(gradient) {
              if (missing(gradient))
                return(private$.gradient)
              assert_that(is.opt.type(gradient,
                                      gradientType))
              private$.gradient <- gradient
            },
            loop = function(loop) {
              if (missing(loop))
                return(private$.loop)
              assert_that(is.opt.type(loop,
                                      loopType))
              private$.loop <- loop
            },
            pause = function(pause) {
              if (missing(pause))
                return(private$.pause)
              assert_that(is.opt.type(pause,
                                      pauseType))
              private$.pause <- pause
            },
            lidOpen = function(lidOpen) {
              if (missing(lidOpen))
                return(private$.lidOpen)
              assert_that(is.opt.type(lidOpen,
                                      lidOpenType))
              private$.lidOpen <- lidOpen
            }
          ))

# thermalCyclingConditionsType ------------------------------------------------------------
thermalCyclingConditionsType <- 
  R6Class("thermalCyclingConditionsType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  description = NULL,
                                  documentation = NULL,
                                  lidTemperature = NULL,
                                  experimenter = NULL,
                                  step) {
              assert_that(is.type(id,
                                  idType))
              assert_that(is.opt.string(description))
              assert_that(is.opt.list.type(documentation,
                                           idReferencesType))
              assert_that(is.opt.double(lidTemperature))
              assert_that(is.opt.list.type(experimenter,
                                           idReferencesType))
              assert_that(is.opt.list.type(step,
                                           stepType))
              
              private$.id <- id
              private$.description <- description
              private$.documentation <- with.names(documentation, id)
              private$.lidTemperature <- lidTemperature
              private$.experimenter <- with.names(experimenter, id)
              private$.step <- with.names(step, id)
                
            }
          ),
          private = list(
            .id = NULL,
            .description = NULL,
            .documentation = NULL,
            .lidTemperature = NULL,
            .experimenter = NULL,
            .step = NULL
          ),
          active = list(
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assert_that(is.type(id, idType))
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
                                           idReferencesType))
              private$.documentation <- with.names(documentation, id)
            },
            lidTemperature = function(lidTemperature) {
              if (missing(lidTemperature))
                return(private$.lidTemperature)
              assert_that(is.opt.double(lidTemperature))
              private$.lidTemperature <- lidTemperature
            },
            experimenter = function(experimenter) {
              if (missing(experimenter))
                return(private$.experimenter)
              assert_that(is.opt.list.type(experimenter,
                                           idReferencesType))
              private$.experimenter <- with.names(experimenter, id)
            },
            step = function(step) {
              if (missing(step))
                return(private$.step)
              assert_that(is.opt.list.type(step,
                                           idReferencesType))
              private$.step <- with.names(step, id)
            }
          ))
