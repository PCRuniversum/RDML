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

#' Base R6 class for RDML package.
#' 
#' Most classes from RDML package inherit this class. It is designed for internal 
#' usage and should not be directly accessed.
#' 
#' @section Initialization: \preformatted{rdmlBaseType$new()}
#'   
#' @section Methods: \describe{
#' \item{\code{.asXMLnodes(node.name)}}{Represents
#'   object as XML nodes. Should not be called directly. \code{node.name} --
#'   name of the root node for the generated XML
#'   tree.}
#' \item{\code{print(...)}}{prints object}}
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
rdmlBaseType <-
  R6Class("rdmlBaseType",
          # class = FALSE,
          public = list(
            .asXMLnodes = function(node.name,
                                    namespaceDefinitions = NULL) {
              subnodes <- names(private)[grepl("^\\..*$",
                                               names(private))] %>% rev
              sprintf("<%s%s>%s</%s>",
                      node.name, #node name
                      # attribute
                      {
                        attrs <- NULL
                        for (subnode in subnodes) {
                          if (class(private[[subnode]])[1] == "idType" ||
                              class(private[[subnode]])[1] == "reactIdType") {
                            attrs <- c(attrs, subnode)
                          }
                        }
                        if (length(attrs) == 0) {
                          ""
                        } else {
                          subnodes <- 
                            setdiff(subnodes, attrs)
                          sprintf(" id = '%s'", private[[attrs[1]]]$id)
                        }
                        # "'attr'"
                      },
                      # value
                      {
                        sapply(
                          subnodes,
                          function(name) {
                            subnode.name <- gsub("^\\.(.*)$",
                                                 "\\1", name)
                            switch(
                              typeof(private[[name]]),
                              closure = NULL,
                              list = 
                                sapply(private[[name]],
                                       function(sublist)
                                         sublist$.asXMLnodes(subnode.name)) %>%
                                # .[!sapply(., is.null)] %>% 
                                paste0(collapse = "\n")
                              ,
                              environment = {
                                private[[name]]$.asXMLnodes(subnode.name)
                              },
                              {
                                if (is.null(private[[name]]) ||
                                    is.na(private[[name]])) {
                                  NULL
                                } else {
                                  sprintf("<%s>%s</%s>\n",
                                          subnode.name,
                                          switch(
                                            typeof(private[[name]]),
                                            logical = 
                                              ifelse(private[[name]],
                                                     "true",
                                                     "false"
                                              ),
                                            private[[name]]
                                          ),
                                          subnode.name
                                  )
                                }
                              })
                          }) %>%
                          .[!sapply(., is.null)] %>% 
                          paste0(collapse = "")
                      }, 
                      node.name)
            },
            print = function(...) {
              elements <- names(private)[-which(names(private) == 
                                                  "deep_clone")] %>% rev
              sapply(elements,
                     function(name) {
                       sprintf(
                         "\t%s: %s",
                         gsub("^\\.(.*)$",
                              "\\1", name),
                         switch(
                           typeof(private[[name]]),
                           closure = NULL,
                           list = sprintf("[%s]",
                                          names(private[[name]]) %>% 
                                            paste(collapse = ", ")),
                           environment = {
                             sprintf("~ %s",
                                     class(private[[name]])[1])
                           },
                           NULL = "",
                           {
                             if (class(private[[name]]) == "matrix")
                               sprintf("%s fluorescence data points",
                                       nrow(private[[name]]))
                             else
                               sprintf("%s", private[[name]])
                           }))
                     }) %>% 
                paste(sep = "\n", collapse = "\n") %>% 
                cat
            }
          ),
          private = list(
            deep_clone = function(name, value) {
              if (value %>% is.null)
                return(NULL)
              if (value %>%
                  class %>% 
                  tail(1) != "R6") {
                if (is.list(value)) {
                  llply(value,
                        function(el) el$clone(deep = TRUE))
                } else {
                  value
                }
              } else {
                value$clone(deep = TRUE)
              }
            }
          )
  )

# rdmlIdType ------------------------------------------------------------

#' rdmlIdType R6 class.
#' 
#' This element can be used to assign a publisher and id to the RDML file.\cr 
#' Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{rdmlIdType$new(publisher, serialNumber,
#'   MD5Hash = NULL)}
#'   
#' @section Fields: \describe{  
#'   \item{\code{publisher}}{\link[checkmate]{checkString}. RDML file publisher.}
#'   \item{\code{serialNumber}}{\link[checkmate]{checkString}. Serial number.} 
#'   \item{\code{MD5Hash}}{\link[checkmate]{checkString}. An MD5Hash calculated
#'   over the complete file after removing all rdmlIDTypes and all whitespaces
#'   between elements.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
rdmlIdType <- 
  R6Class("rdmlIdType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(publisher,
                                  serialNumber,
                                  MD5Hash = NULL) {
              assertString(publisher)
              assertString(serialNumber)
              assert(checkNull(MD5Hash),
                     checkString(MD5Hash))
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
              assertString(publisher)
              private$.publisher <- publisher
            },
            serialNumber = function(serialNumber) {
              if (missing(serialNumber))
                return(private$.serialNumber)
              assertString(serialNumber)
              private$.serialNumber <- serialNumber
            },
            MD5Hash = function(MD5Hash) {
              if (missing(MD5Hash))
                return(private$.MD5Hash)
              assert(checkNull(MD5Hash),
                     checkString(MD5Hash))
              private$.MD5Hash <- MD5Hash
            }
          ))

# idType ------------------------------------------------------------

#' idType R6 class.
#' 
#' Contains identificator for varius RDML types. Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{idType$new(id)}
#'
#'   @section Fields: \describe{     
#' \item{\code{id}}{\link[checkmate]{checkString}. Identificator.}
#' }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
idType <- 
  R6Class("idType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id) {
              assertString(id)
              private$.id <- id
            },
            .asXMLnodes = function(node.name) {
              sprintf("<%s id = '%s'/>",
                      node.name,
                      private$.id)
              }
            #                         print = function(...) {
            #                           cat(private$.id)
            #                         }
          ),
          private = list(
            .id = NULL
          ),
          active = list(
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assertString(id)
              private$.id <- id
            }
          ))

# reactIdType ------------------------------------------------------------

#' reactIdType R6 class.
#' 
#' Contains identificator for reactType. Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{reactIdType$new(id)}
#'
#'   @section Fields: \describe{     
#' \item{\code{id}}{\link[checkmate]{checkCount}. Identificator.}
#' }
#' 
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
reactIdType <- 
  R6Class("reactIdType",
          # class = FALSE,
          inherit = idType,
          public = list(
            initialize = function(id) {
              assertCount(id)
              private$.id <- id
            }#,
            #             print = function(...) {
            #               cat(private$.id)
            #             }
          ),
          private = list(
            .id = NULL
          ),
          active = list(
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assertCount(id)
              private$.id <- id
            }
          ))

# idReferencesType ------------------------------------------------------------

#' idReferencesType R6 class.
#' 
#' Contains id of another RDML object. Inherits: \link{idType}.
#' 
#' @section Initialization: \preformatted{idReferencesType$new(id)}
#' 
#' @section Fields: \describe{  
#' \item{\code{id}}{\link[checkmate]{checkString}. Identificator.}
#' }
#'         
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
idReferencesType <- 
  R6Class("idReferencesType",
          # class = FALSE,
          inherit = idType)

# experimenterType ------------------------------------------------------------

#' experimenterType R6 class.
#' 
#' Contact details of the experimenter. Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{experimenterType$new(id, firstName, lastName,
#'   email = NULL, labName = NULL, labAddress = NULL)}
#'  
#'   @section Fields: \describe{   
#' \item{\code{id}}{\link{idType}. Identificator.}
#' \item{\code{firstName}}{\link[checkmate]{checkString}. First name.}
#' \item{\code{lastName}}{\link[checkmate]{checkString}. Last name.}
#' \item{\code{email}}{\link[checkmate]{checkString}. Email.}
#' \item{\code{labName}}{\link[checkmate]{checkString}. Lab name.}
#' \item{\code{labAddress}}{\link[checkmate]{checkString}. Lab address.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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
              assertClass(id, "idType")
              assertString(firstName)
              assertString(lastName)
              assert(checkNull(email),
                     checkString(email))
              assert(checkNull(labName),
                     checkString(labName))
              assert(checkNull(labAddress),
                     checkString(labAddress))
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
              assertClass(id, "idType")
              private$.id <- id
            },
            firstName = function(firstName) {
              if (missing(firstName))
                return(private$.firstName)
              assertString(firstName)
              private$.firstName <- firstName
            },
            lastName = function(lastName) {
              if (missing(lastName))
                return(private$.lastName)
              assertString(lastName)
              private$.lastName <- lastName
            },
            email = function(email) {
              if (missing(email))
                return(private$.email)
              assert(checkNull(email),
                     checkString(email))
              private$.email <- email
            },
            labName = function(labName) {
              if (missing(labName))
                return(private$.labName)
              assert(checkNull(labName),
                     checkString(labName))
              private$.labName <- labName
            },
            labAddress = function(labAddress) {
              if (missing(labAddress))
                return(private$.labAddress)
              assert(checkNull(labAddress),
                     checkString(labAddress))
              private$.labAddress <- labAddress
            }
          ))

# documentationType ------------------------------------------------------------

#' documentationType R6 class.
#' 
#' These elements should be used if the same description applies to many
#' samples, targets or experiments. Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{documentationType$new(id, text = NULL)}
#'   
#'   @section Fields: \describe{  
#' \item{\code{id}}{\link{idType}. Identificator.}
#' \item{\code{text}}{\link[checkmate]{checkString}. Text.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
documentationType <- 
  R6Class("documentationType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  text = NULL) {
              assertClass(id, "idType")
              assert(checkNull(text),
                     checkString(text))
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
              assertClass(id, "idType")
              private$.id <- id
            },
            text = function(text) {
              if (missing(text))
                return(private$.text)
              assert(checkNull(text),
                     checkString(text))
              private$.text <- text
            }
          ))

# dyeType ------------------------------------------------------------

#' dyeType R6 class.
#' 
#' Detailed information about the dye. Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{dyeType$new(id, description = NULL)}
#'   
#'   @section Fields: \describe{  
#' \item{\code{id}}{\link{idType}. Identificator.}
#' \item{\code{description}}{ \link[checkmate]{checkString}. Description.
#'   }}
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
dyeType <- 
  R6Class("dyeType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  description = NULL) {
              assertClass(id, "idType")
              assert(checkNull(description),
                     checkString(description))
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
              assertClass(id, "idType")
              private$.id <- id
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert(checkNull(description),
                     checkString(description))
              private$.description <- description
            }
          ))

# xRefType ------------------------------------------------------------

#' xRefType R6 class.
#' 
#' Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{xRefType$new(name = NULL, id = NULL)}
#'   
#'   @section Fields: \describe{  
#'   \item{\code{name}}{\link[checkmate]{checkString}. Reference to an external
#'   database, } for example "GenBank". 
#'   \item{\code{id}}{\link[checkmate]{checkString}. The ID of the entry within
#'   the external database, for example "AJ832138".}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
xRefType <- 
  R6Class("xRefType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(name = NULL,
                                  id = NULL) {
              assert(checkNull(name),
                     checkString(name))
              assert(checkNull(id),
                     checkString(id))
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
              assert(checkNull(name),
                     checkString(name))
              private$.name <- name
            },
            id = function(id) {
              if (missing(id))
                return(private$.id)
              assert(checkNull(id),
                     checkString(id))
              private$.id <- id
            }
          ))

# annotationType ------------------------------------------------------------

#' annotationType R6 class.
#' 
#' Annotate samples by setting a property and its value. For example, 
#' sex could be a property with the possible values M or F. Inherits:
#' \link{rdmlBaseType}.
#' 
#' @section Fields: \describe{ \item{property}{\link[checkmate]{checkString}.
#'   Property name} \item{value}{\link[checkmate]{checkString}. Value} }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
#' @examples
#' #set sex property
#' annotationType$new(property = "sex", value = "M")
annotationType <- 
  R6Class("annotationType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(property,
                                  value) {
              assertString(property)
              assertString(value)
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
              assertString(property)
              private$.property <- property
            },
            value = function(value) {
              if (missing(value))
                return(private$.value)
              assertString(value)
              private$.value <- value
            }
          ))

# quantityType ------------------------------------------------------------

#' quantityType R6 class.
#' 
#' A quantity is always defined by its value and its unit. Inherits:
#' \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{quantityType$new(value, unit)}
#' 
#'   @section Fields: \describe{    
#' \item{\code{value}}{\link[checkmate]{checkNumber}. Value.}
#' \item{\code{unit}}{\link{quantityUnitType}. Unit.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
quantityType <- 
  R6Class("quantityType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(value,
                                  unit) {
              assertNumber(value)
              assertClass(unit, "quantityUnitType")
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
              assertNumber(value)
              private$.value <- value
            },
            unit = function(unit) {
              if (missing(unit))
                return(private$.unit)
              assertClass(unit, "quantityUnitType")
              private$.unit <- unit
            }
          ))

# cdnaSynthesisMethodType ------------------------------------------------------------

#' cdnaSynthesisMethodType R6 class.
#' 
#' Description of the cDNA synthesis method. Inherits: \link{rdmlBaseType}.
#' 
#' @section 
#' Initialization: \preformatted{cdnaSynthesisMethodType$new(enzyme = NULL, 
#'   primingMethod = NULL, dnaseTreatment = NULL, thermalCyclingConditions = 
#'   NULL)}
#'   
#'   @section Fields: \describe{  
#'   \item{\code{enzyme}}{\link[checkmate]{checkString}. Name of the enzyme used for 
#'   reverse transcription.} 
#'   \item{\code{primingMethod}}{\link{primingMethodType}.} 
#'   \item{\code{dnaseTreatment}}{\link[checkmate]{checkFlag} if \code{TRUE}RNA was
#'   DNAse treated prior cDNA synthesis.}
#'   \item{\code{thermalCyclingConditions}}{\link{idReferencesType}.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
cdnaSynthesisMethodType <- 
  R6Class("cdnaSynthesisMethodType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(enzyme = NULL,
                                  primingMethod = NULL,
                                  dnaseTreatment = NULL,
                                  thermalCyclingConditions = NULL) {
              assert(checkNull(enzyme),
                     checkString(enzyme))
              assert(checkNull(primingMethod),
                     checkClass(primingMethod, "primingMethodType"))
              assert(checkNull(dnaseTreatment),
                     checkFlag(dnaseTreatment))
              assert(checkNull(thermalCyclingConditions),
                     checkClass(thermalCyclingConditions, "idType"))
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
              assert(checkNull(enzyme),
                     checkString(enzyme))
              private$.enzyme <- enzyme
            },
            primingMethod = function(primingMethod) {
              if (missing(primingMethod))
                return(private$.primingMethod)
              assert(checkNull(primingMethod),
                     checkClass(primingMethod, "primingMethodType"))
              private$.primingMethod <- primingMethod
            },
            dnaseTreatment = function(dnaseTreatment) {
              if (missing(dnaseTreatment))
                return(private$.dnaseTreatment)
              assert(checkNull(dnaseTreatment),
                     checkFlag(dnaseTreatment))
              private$.dnaseTreatment <- dnaseTreatment
            },
            thermalCyclingConditions = function(thermalCyclingConditions) {
              if (missing(thermalCyclingConditions))
                return(private$.thermalCyclingConditions)
              assert(checkNull(thermalCyclingConditions),
                     checkClass(thermalCyclingConditions, "idType"))
              private$.thermalCyclingConditions <- thermalCyclingConditions
            }
          ))

# templateQuantityType ------------------------------------------------------------

#' templateQuantityType R6 class.
#' 
#' Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{templateQuantityType$new(conc, nucleotide)}
#'   
#'   @section Fields: \describe{  
#' \item{\code{conc}}{\link[checkmate]{checkNumber}. Concentration of the template in nanogram}
#'   per microliter in the final reaction mix.
#' \item{\code{nucleotide}}{\link{nucleotideType}.}
#'   }
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
templateQuantityType <- 
  R6Class("templateQuantityType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(conc,
                                  nucleotide) {
              assertNumber(conc)
              assertClass(nucleotide, "nucleotideType")
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
              assertNumber(conc)
              private$.conc <- conc
            },
            nucleotide = function(nucleotide) {
              if (missing(nucleotide))
                return(private$.nucleotide)
              assertClass(nucleotide, "nucleotideType")
              private$.nucleotide <- nucleotide
            }
          ))

# enumType ------------------------------------------------------------

#' enumType R6 class.
#' 
#' Generic class for creating objects thet can take limited list of values. \cr
#' Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{enumType$new(value)}
#'   @section Fields: \describe{  
#' \item{\code{value}}{\link[checkmate]{checkString}. Value.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
enumType <- 
  R6Class("enumType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(value, ...) {
              assert(checkNull(value), checkChoice(value, c(private$.levels)))
              private$.value <- value
            },
            print = function(...) {
              cat(private$.value)
            },
            .asXMLnodes = function(node.name,
                                   namespaceDefinitions = NULL) {
              if (is.null(private$.value) ||
                  is.na(private$.value))
                NULL
              else
                sprintf("<%s>%s</%s>",
                        node.name,
                        private$.value,
                        node.name)
            }
          ),
          private = list(
            .value = NULL
          ),
          active = list(
            value = function(value) {
              if (missing(value))
                return(private$.value)
              assertChoice(value, c(private$.levels))
              private$.value <- value
            }
          ))

# sampleTypeType ------------------------------------------------------------

#' sampleTypeType R6 class.
#' 
#' Can take values:
#' \describe{
#' \item{unkn}{unknown sample}
#' \item{ntc}{non template control}
#' \item{nac}{no amplification control}
#' \item{std}{standard sample}
#' \item{ntp}{no target present}
#' \item{nrt}{minusRT}
#' \item{pos}{positive control}
#' \item{opt}{optical calibrator sample}}
#' 
#' Inherits: \link{enumType}.
#' 
#' @section Initialization: \preformatted{sampleTypeType$new(value)}
#'  
#'   @section Fields: \describe{   
#' \item{\code{value}}{\link[checkmate]{checkString}. Value.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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

#' quantityUnitType R6 class.
#' 
#' The unit the quantity. Can take values:
#' \describe{
#' \item{cop}{copies per microliter  }
#' \item{fold}{fold change  }
#' \item{dil}{dilution (10 would mean 1:10 dilution)  }
#' \item{nMol}{nanomol per microliter }
#' \item{ng}{nanogram per microliter }
#' \item{other}{other unit (must be linear, no exponents or logarithms allowed) }
#' }
#' 
#' Inherits: \link{enumType}.
#' 
#' @section Initialization: \preformatted{quantityUnitType$new(value)}
#'   
#'   @section Fields: \describe{  
#' \item{\code{value}}{\link[checkmate]{checkString}. Value.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
quantityUnitType <- 
  R6Class("quantityUnitType",
          # class = FALSE,
          inherit = enumType,
          private = list(
            .levels = c("cop", "fold", "dil", "ng", "nMol", "other")
          )
  )

# primingMethodType ------------------------------------------------------------

#' primingMethodType R6 class.
#' 
#' The primers used in the reverse transcription. Can take values:
#' \describe{
#' \item{oligo-dt}{}
#' \item{random}{}
#' \item{target-specific}{}
#' \item{oligo-dt and random}{}
#' \item{other}{} 
#' }
#' 
#' Inherits: \link{enumType}.
#' 
#' @section Initialization: \preformatted{primingMethodType$new(value)}
#'   
#'   @section Fields: \describe{  
#' \item{\code{value}}{\link[checkmate]{checkString}. Value.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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

#' nucleotideType R6 class.
#' 
#' Type of nucleic acid used as a template in the experiment. May have following values:
#' \describe{
#' \item{DNA}{}
#' \item{genomic DNA}{}
#' \item{cDNA}{}
#' \item{RNA}{}
#' }
#' 
#' Inherits: \link{enumType}.
#' 
#' @section Initialization: \preformatted{nucleotideType$new(value)}
#'   
#'   @section Fields: \describe{  
#' \item{\code{value}}{\link[checkmate]{checkString}. Value.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
nucleotideType <- 
  R6Class("nucleotideType",
          # class = FALSE,
          inherit = enumType,
          private = list(
            .levels = c("DNA",
                        "genomic DNA",
                        "cDNA",
                        "RNA")
          )
  )

# sampleType ------------------------------------------------------------

#'sampleType R6 class.
#'
#' A sample is a template solution with defined concentation. Since dilutions 
#' of the same material differ in concentration, they are considered different 
#' samples. A technical replicate samples should contain the same name (reactions 
#' are performed on the same material), and biological replicates should contain 
#' different names (the template derived from the different biological replicates 
#' is are divergent). Serial dilutions in a standard curve must have different 
#' names (preferably stating their dillution). 
#' Inherits: \link{rdmlBaseType}.
#'
#'@section Initialization: \preformatted{sampleType$new(id, description = NULL, 
#'  documentation = NULL, xRef =  NULL, annotation = NULL, type = 
#'  sampleTypeType$new("unkn"), interRunCalibrator = FALSE, quantity = NULL, 
#'  calibratorSample = FALSE, cdnaSynthesisMethod = NULL, templateQuantity = 
#'  NULL)}
#'  
#'  @section Fields: \describe{  
#'  \item{\code{id}}{\link{idType}. Concentration of the template in nanogram
#'  per microliter in the final reaction mix. }
#'  \item{\code{description}}{\link[checkmate]{checkString}.} 
#'  \item{\code{documentation}}{\code{list} of \link{idReferencesType}.} 
#'  \item{\code{xRef}}{\code{list} of \link{xRefType}.} 
#'  \item{\code{annotation}}{\code{list} of \link{annotationType}.} 
#'  \item{\code{type}}{\link{sampleTypeType}.} 
#'  \item{\code{interRunCalibrator}}{\link[checkmate]{checkFlag}. \code{TRUE} 
#'  if this sample is used as inter run calibrator. }
#'  \item{\code{quantity}}{\link{quantityType}. Quantity - The reference
#'  quantity of this sample. It should be only used if the sample is part of a
#'  standard curve. The provided value will be used to quantify unknown samples
#'  in absolute quantification assays. Only the use of positive integers (like 1, 
#'  10, 100, 1000) and fractions (e.g. 1, 0.1, 0.01, 0.001) is acceptable. 
#'  The use of exponents (1, 2, 3, 4 or -1, -2, -3, -4) if forbidden, 
#'  because it will not be interpreted as 10E1, 10E2, 10E3, 10E4 or 10E-1, 10E-2, 
#'  10E-3, 10E-4. }
#'  \item{\code{calibratorSample}}{\link[checkmate]{checkFlag}. \code{TRUE} if this
#'  sample is used as calibrator sample. }
#'  \item{\code{cdnaSynthesisMethod}}{\link{cdnaSynthesisMethodType}.} 
#'  \item{\code{templateQuantity}}{\link{templateQuantityType}.}
#'  }
#'  
#'@docType class
#'@format An \code{\link{R6Class}} generator object.
#'@export
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
              assertClass(id, "idType")
              assert(checkNull(description),
                     checkString(description))
              assert(checkNull(documentation),
                     checkList(documentation, "idReferencesType"))
              assert(checkNull(xRef),
                     checkList(xRef, "xRefType"))
              assert(checkNull(annotation),
                     checkList(annotation, "annotationType"))
              assertClass(type, "sampleTypeType")
              assert(checkNull(interRunCalibrator),
                     checkFlag(interRunCalibrator))
              assert(checkNull(quantity),
                     checkClass(quantity, "quantityType"))
              assert(checkNull(calibratorSample),
                     checkFlag(calibratorSample))
              assert(checkNull(cdnaSynthesisMethod),
                     checkClass(cdnaSynthesisMethod, "cdnaSynthesisMethodType"))
              assert(checkNull(templateQuantity),
                     checkClass(templateQuantity, "templateQuantityType"))
              
              private$.id <- id
              private$.description <- description
              private$.documentation <- documentation
              private$.xRef <- xRef
              private$.annotation <- with.names(annotation,
                                                quote(.$property))
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
              assertClass(id, "idType")
              private$.id <- id
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert(checkNull(description),
                     checkString(description))
              private$.description <- description
            },
            documentation = function(documentation) {
              if (missing(documentation))
                return(private$.documentation)
              assert(checkNull(documentation),
                     checkList(documentation, "idReferencesType"))
              private$.documentation <- documentation
            },
            xRef = function(xRef) {
              if (missing(xRef))
                return(private$.xRef)
              assert(checkNull(xRef),
                     checkList(xRef, "xRefType"))
              private$.xRef <- xRef
            },
            annotation = function(annotation) {
              if (missing(annotation))
                return(private$.annotation)
              assert(checkNull(annotation),
                     checkList(annotation, "annotationType"))
              private$.annotation <- with.names(annotation,
                                                quote(.$property))
            },
            type = function(type) {
              if (missing(type))
                return(private$.type)
              assertClass(type, "sampleTypeType")
              private$.type <- type
            },
            interRunCalibrator = function(interRunCalibrator) {
              if (missing(interRunCalibrator))
                return(private$.interRunCalibrator)
              assert(checkNull(interRunCalibrator),
                     checkFlag(interRunCalibrator))
              private$.interRunCalibrator <- interRunCalibrator
            },
            quantity = function(quantity) {
              if (missing(quantity))
                return(private$.quantity)
              assert(checkNull(quantity),
                     checkClass(quantity, "quantityType"))
              private$.quantity <- quantity
            },
            calibratorSample = function(calibratorSample) {
              if (missing(calibratorSample))
                return(private$.calibratorSample)
              assert(checkNull(calibratorSample),
                     checkFlag(calibratorSample))
              private$.calibratorSample <- calibratorSample
            },
            cdnaSynthesisMethod = function(cdnaSynthesisMethod) {
              if (missing(cdnaSynthesisMethod))
                return(private$.cdnaSynthesisMethod)
              assert(checkNull(cdnaSynthesisMethod),
                     checkClass(cdnaSynthesisMethod, "cdnaSynthesisMethodType"))
              private$.cdnaSynthesisMethod <- cdnaSynthesisMethod
            },
            templateQuantity = function(templateQuantity) {
              if (missing(templateQuantity))
                return(private$.templateQuantity)
              assert(checkNull(templateQuantity),
                     checkClass(templateQuantity, "templateQuantityType"))
              private$.templateQuantity <- templateQuantity
            }
          ))

# oligoType ------------------------------------------------------------

#' oligoType R6 class.
#' 
#' Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{oligoType$new(threePrimeTag = NULL, 
#'   fivePrimeTag = NULL, sequence)}
#'   
#'   @section Fields: \describe{  
#'   \item{\code{threePrimeTag}}{\link[checkmate]{checkString}. Description of
#'   three prime modification (if present). }
#'   \item{\code{fivePrimeTag}}{\link[checkmate]{checkString}. Description of
#'   five prime modification (if present).}
#'   \item{\code{sequence}}{\link[checkmate]{checkString}.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
oligoType <- 
  R6Class("oligoType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(threePrimeTag = NULL,
                                  fivePrimeTag = NULL,
                                  sequence) {
              assert(checkNull(threePrimeTag),
                     checkString(threePrimeTag))
              assert(checkNull(fivePrimeTag),
                     checkString(fivePrimeTag))
              assertString(sequence)
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
              assert(checkNull(threePrimeTag),
                     checkString(threePrimeTag))
              private$.threePrimeTag <- threePrimeTag
            },
            fivePrimeTag = function(fivePrimeTag) {
              if (missing(fivePrimeTag))
                return(private$.fivePrimeTag)
              assert(checkNull(fivePrimeTag),
                     checkString(fivePrimeTag))
              private$.fivePrimeTag <- fivePrimeTag
            },
            sequence = function(sequence) {
              if (missing(sequence))
                return(private$.sequence)
              assertString(sequence)
              private$.sequence <- sequence
            }
          ))

# sequencesType ------------------------------------------------------------

#' sequencesType R6 class.
#' 
#' Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{sequencesType$new(forwardPrimer = NULL, 
#' reversePrimer = NULL, probe1 = NULL, probe2 = NULL, amplicon = NULL)}
#'   
#'   @section Fields: \describe{  
#' \item{\code{forwardPrimer}}{\link{oligoType}.}
#' \item{\code{reversePrimer}}{\link{oligoType}.}
#' \item{\code{probe1}}{\link{oligoType}.}
#' \item{\code{probe2}}{\link{oligoType}.}
#' \item{\code{amplicon}}{\link{oligoType}.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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
              assert(checkNull(forwardPrimer),
                     checkClass(forwardPrimer, "oligoType"))
              assert(checkNull(reversePrimer),
                     checkClass(reversePrimer, "oligoType"))
              assert(checkNull(probe1),
                     checkClass(probe1, "oligoType"))
              assert(checkNull(probe2),
                     checkClass(probe2, "oligoType"))
              assert(checkNull(amplicon),
                     checkClass(amplicon, "oligoType"))
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
              assert(checkNull(forwardPrimer),
                     checkClass(forwardPrimer, "oligoType"))
              private$.forwardPrimer <- forwardPrimer
            },
            reversePrimer = function(reversePrimer) {
              if (missing(reversePrimer))
                return(private$.reversePrimer)
              assert(checkNull(reversePrimer),
                     checkClass(reversePrimer, "oligoType"))
              private$.reversePrimer <- reversePrimer
            },
            probe1 = function(probe1) {
              if (missing(probe1))
                return(private$.probe1)
              assert(checkNull(probe1),
                     checkClass(probe1, "oligoType"))
              private$.probe1 <- probe1
            },
            probe2 = function(probe2) {
              if (missing(probe2))
                return(private$.probe2)
              assert(checkNull(probe2),
                     checkClass(probe2, "oligoType"))
              private$.probe2 <- probe2
            },
            amplicon = function(amplicon) {
              if (missing(amplicon))
                return(private$.amplicon)
              assert(checkNull(amplicon),
                     checkClass(amplicon, "oligoType"))
              private$.amplicon <- amplicon
            }
          ))

# commercialAssayType ------------------------------------------------------------

#' commercialAssayType R6 class.
#' 
#' For some commercial assays, the primer sequences may be unknown. This element 
#' allows to describe commercial assays. Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{commercialAssayType$new(company, orderNumber)}
#'   
#'   @section Fields: \describe{  
#' \item{\code{company}}{\link[checkmate]{checkString}.}
#' \item{\code{orderNumber}}{\link[checkmate]{checkString}.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
commercialAssayType <- 
  R6Class("commercialAssayType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(company,
                                  orderNumber) {
              assertString(company)
              assertString(orderNumber)
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
              assertString(company)
              private$.company <- company
            },
            orderNumber = function(orderNumber) {
              if (missing(orderNumber))
                return(private$.orderNumber)
              assertString(orderNumber)
              private$.orderNumber <- orderNumber
            }
          ))

# targetTypeType ------------------------------------------------------------

#' targetTypeType R6 class.
#' 
#' Can take values:
#' \describe{
#' \item{ref}{reference target}
#' \item{toi}{target of interest}
#' } 
#' Inherits: \link{enumType}.
#' 
#' @section Initialization: \preformatted{targetTypeType$new(value)}
#' 
#'   @section Fields: \describe{  
#' \item{\code{value}}{\link[checkmate]{checkString}.}
#' }
#'  
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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

#' targetType R6 class.
#' 
#' A target is a PCR reaction with defined set of primers. PCR reactions 
#' for the same gene with distinct primer sequences are considered different 
#' targets. Inherits: 
#' \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{targetType$new(id, description = NULL, 
#'   documentation = NULL, xRef = NULL, type, amplificationEfficiencyMethod = 
#'   NULL, amplificationEfficiency = NULL, amplificationEfficiencySE = NULL, 
#'   detectionLimit = NULL, dyeId, sequences = NULL, commercialAssay = NULL)}
#'   
#' @section Fields: \describe{ \item{\code{id}}{\link{idType}.} 
#'   \item{\code{description}}{\link[checkmate]{checkString}.} 
#'   \item{\code{documentation}}{\code{list} of \link{idReferencesType}.} 
#'   \item{\code{xRef}}{\code{list} of \link{xRefType}.} 
#'   \item{\code{type}}{\link{targetTypeType}.} 
#'   \item{\code{amplificationEfficiencyMethod}}{\link[checkmate]{checkString}.} 
#'   \item{\code{amplificationEfficiency}}{\link[checkmate]{checkNumber}.} 
#'   \item{\code{amplificationEfficiencySE}}{\link[checkmate]{checkNumber}.} 
#'   \item{\code{detectionLimit}}{\link[checkmate]{checkNumber}.} 
#'   \item{\code{dyeId}}{\link{idReferencesType}.} 
#'   \item{\code{sequences}}{\link{sequencesType}.} 
#'   \item{\code{commercialAssay}}{\link{commercialAssayType}.} }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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
              assertClass(id, "idType")
              assert(checkNull(description),
                     checkString(description))
              assert(checkNull(documentation),
                     checkList(documentation, "idReferencesType"))
              assert(checkNull(xRef),
                     checkList(xRef, "xRefType"))
              assertClass(type, "targetTypeType")
              assert(checkNull(amplificationEfficiencyMethod),
                     checkString(amplificationEfficiencyMethod))
              assert(checkNull(amplificationEfficiency),
                     checkNumber(amplificationEfficiency))
              assert(checkNull(amplificationEfficiencySE),
                     checkNumber(amplificationEfficiencySE))
              assert(checkNull(detectionLimit),
                     checkNumber(detectionLimit))
              assertClass(dyeId, "idReferencesType")
              assert(checkNull(sequences),
                     checkClass(sequences, "sequencesType"))
              assert(checkNull(commercialAssay),
                     checkClass(commercialAssay, "commercialAssayType"))
              
              private$.id <- id
              private$.description <- description
              private$.documentation <- documentation
              private$.xRef <- xRef
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
              assertClass(id, "idType")
              private$.id <- id
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert(checkNull(description),
                     checkString(description))
              private$.description <- description
            },
            documentation = function(documentation) {
              if (missing(documentation))
                return(private$.documentation)
              assert(checkNull(documentation),
                     checkList(documentation, "idReferencesType"))
              private$.documentation <- documentation
            },
            xRef = function(xRef) {
              if (missing(xRef))
                return(private$.xRef)
              assert(checkNull(xRef),
                     checkList(xRef, "xRefType"))
              private$.xRef <- xRef
            },
            type = function(type) {
              if (missing(type))
                return(private$.type)
              assertClass(type, "targetTypeType")
              private$.type <- type
            },
            amplificationEfficiencyMethod = 
              function(amplificationEfficiencyMethod) {
                if (missing(amplificationEfficiencyMethod))
                  return(private$.amplificationEfficiencyMethod)
                assert(checkNull(amplificationEfficiencyMethod),
                       checkString(amplificationEfficiencyMethod))
                private$.amplificationEfficiencyMethod <- amplificationEfficiencyMethod
              },
            amplificationEfficiency = function(amplificationEfficiency) {
              if (missing(amplificationEfficiency))
                return(private$.amplificationEfficiency)
              assert(checkNull(amplificationEfficiency),
                     checkNumber(amplificationEfficiency))
              private$.amplificationEfficiency <- amplificationEfficiency
            },
            amplificationEfficiencySE = function(amplificationEfficiencySE) {
              if (missing(amplificationEfficiencySE))
                return(private$.amplificationEfficiencySE)
              assert(checkNull(amplificationEfficiencySE),
                     checkNumber(amplificationEfficiencySE))
              private$.amplificationEfficiencySE <- amplificationEfficiencySE
            },
            detectionLimit = function(detectionLimit) {
              if (missing(detectionLimit))
                return(private$.detectionLimit)
              assert(checkNull(detectionLimit),
                     checkNumber(detectionLimit))
              private$.detectionLimit <- detectionLimit
            },
            dyeId = function(dyeId) {
              if (missing(dyeId))
                return(private$.dyeId)
              assertClass(dyeId, "idReferencesType")
              private$.dyeId <- dyeId
            },
            sequences = function(sequences) {
              if (missing(sequences))
                return(private$.sequences)
              assert(checkNull(sequences),
                     checkClass(sequences, "sequencesType"))
              private$.sequences <- sequences
            },
            commercialAssay = function(commercialAssay) {
              if (missing(commercialAssay))
                return(private$.commercialAssay)
              assert(checkNull(commercialAssay),
                     checkClass(commercialAssay, "commercialAssayType"))
              private$.commercialAssay <- commercialAssay
            }
          ))

# # dpAmpCurveType ------------------------------------------------------------
# dpAmpCurveType <- 
#   R6Class("dpAmpCurveType",
#           # class = FALSE,
#           inherit = rdmlBaseType,
#           public = list(
#             initialize = function(cyc,
#                                   tmp = NULL,
#                                   fluor) {
#               assertNumber(cyc)
#               assert(checkNull(tmp),
# checkNumber(tmp))
#               assertNumber(fluor)
#               private$.cyc <- cyc
#               private$.tmp <- tmp
#               private$.fluor <- fluor
#             },
#             AsVector = function() {
#               c(cyc = private$.cyc,
#                 {
#                   if (!is.null(private$.tmp))
#                     c(tmp = private$.tmp)
#                   },
#                 fluor = private$.fluor)
#             }
#           ),
#           private = list(
#             .cyc = NULL,
#             .tmp = NULL,
#             .fluor = NULL
#           ),
#           active = list(
#             cyc = function(cyc) {
#               if (missing(cyc))
#                 return(private$.cyc)
#               assertNumber(cyc)
#               private$.cyc <- cyc
#             },
#             tmp = function(tmp) {
#               if (missing(tmp))
#                 return(private$.tmp)
#               assert(checkNull(tmp),
# checkNumber(tmp))
#               private$.tmp <- tmp
#             },
#             fluor = function(fluor) {
#               if (missing(fluor))
#                 return(private$.fluor)
#               assertNumber(fluor)
#               private$.fluor <- fluor
#             }
#           ))
# 

# # dpMeltingCurveType ------------------------------------------------------------
# dpMeltingCurveType <- 
#   R6Class("dpMeltingCurveType",
#           # class = FALSE,
#           inherit = rdmlBaseType,
#           public = list(
#             initialize = function(tmp,
#                                   fluor) {
#               assertNumber(tmp)
#               assertNumber(fluor)
#               private$.tmp <- tmp
#               private$.fluor <- fluor
#             },
#             AsVector = function() {
#               c(cyc = private$.tmp,
#                 fluor = private$.fluor)
#             }
#           ),
#           private = list(
#             .cyc = NULL,
#             .tmp = NULL,
#             .fluor = NULL
#           ),
#           active = list(
#             tmp = function(tmp) {
#               if (missing(tmp))
#                 return(private$.tmp)
#               assertNumber(tmp)
#               private$.tmp <- tmp
#             },
#             fluor = function(fluor) {
#               if (missing(fluor))
#                 return(private$.fluor)
#               assertNumber(fluor)
#               private$.fluor <- fluor
#             }
#           ))

# adpsType ------------------------------------------------------------

#' adpsType R6 class.
#' 
#' @details 
#' Contains \code{matrix} of amplification data. Must have three columns: \describe{ 
#' \item{cyc}{PCR cycle at which data 
#' point was collected (every cycle must have unique number).} 
#' \item{tmp}{temperature in degrees Celsius at the time of measurement (optional).} 
#' \item{fluor}{raw fluorescence intensity measured.}} Inherits: 
#' \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{adpsType$new(fpoints)}
#' 
#' @section Fields: \describe{    
#'   \item{\code{fpoints}}{\link[checkmate]{assertMatrix}. Matrix with amplification data
#'   points.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
#' @examples 
#' #cycles
#' cyc <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
#' 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 
#' 34, 35, 36, 37, 38, 39, 40)
#' #fluorescence
#' fluo <- c(2.0172, 2.0131, 2.0035, 2, 2.0024, 2.0056, 2.0105, 2.0179, 
#' 2.0272, 2.0488, 2.0922, 2.1925, 2.3937, 2.7499, 3.3072, 4.0966, 
#' 5.0637, 6.0621, 7.0239, 7.8457, 8.5449, 9.1282, 9.6022, 9.9995, 
#' 10.2657, 10.4989, 10.6813, 10.8209, 10.9158, 10.9668, 11.0053, 
#' 11.0318, 11.0446, 11.044, 11.0052, 10.9671, 10.9365, 10.9199, 
#' 10.897, 10.8316)
#' #temperature
#' temp <- c(55, 55, 55, 55, 54, 54, 55, 55, 55, 55, 55, 55, 55, 55, 55, 
#' 55, 55, 55, 55, 55, 55, 55, 55, 56, 55, 55, 55, 55, 55, 55, 55, 
#' 55, 55, 55, 55, 55, 55, 55, 55, 55)
#' 
#' #combine all variables into a proper object
#' data <- matrix(c(cyc, temp, fluo), ncol = 3)
#' colnames(data) <- c("cyc", "tmp", "fluor")
#' 
#' #create adps object
#' adpsType$new(data)
#' 
#' #create adps object without temperature data
#' adpsType$new(data[, -2])
adpsType <- 
  R6Class("adpsType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(fpoints) {
              assert(
                length(setdiff(colnames(fpoints), c("cyc", "tmp", "fluor"))) == 0||
                  length(setdiff(colnames(fpoints), c("cyc", "fluor"))) == 0)
              assertMatrix(fpoints, mode = "numeric")
              private$.fpoints <- fpoints
            },
            .asXMLnodes = function(node.name) {
              apply(private$.fpoints,
                    1,
                    function(fpoints.row) {
                      sprintf("<adp><cyc>%s</cyc>%s<fluor>%s</fluor></adp>",
                              fpoints.row["cyc"],
                              {
                                if(!is.na(fpoints.row["tmp"]))
                                  sprintf("<tmp>%s</tmp>",
                                          fpoints.row["tmp"])
                                else ""
                              },
                              fpoints.row["fluor"])
                    }) %>% paste0(collapse = "")
            }
          ),
          private = list(
            .fpoints = NULL
          ),
          active = list(
            fpoints = function(fpoints) {
              if (missing(fpoints))
                return(private$.fpoints)
              assert(
                length(setdiff(colnames(fpoints), c("cyc", "tmp", "fluor"))) == 0||
                  length(setdiff(colnames(fpoints), c("cyc", "fluor"))) == 0)
              assertMatrix(fpoints, mode = "numeric")
              private$.fpoints <- fpoints
            }
          ))

# mdpsType ------------------------------------------------------------

#' mdpsType R6 class.
#' 
#' Contains \code{matrix} of melting data points (single data points measured
#' during amplification). 
#' 
#' Columns: \describe{ 
#' \item{tmp}{(temperature in degrees Celsius at the time of measurement. 
#' Every point must have unique value.} 
#' \item{fluor}{fluorescence intensity measured without any correction 
#' (including baselining).}} 
#' 
#' Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{mdpsType$new(fpoints)}
#'   
#'   @section Fields: \describe{  
#' \item{\code{fpoints}}{\link[checkmate]{assertMatrix}. Matrix with amplification data points.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
mdpsType <- 
  R6Class("mdpsType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(fpoints) {
              assert(length(setdiff(colnames(fpoints), c("tmp", "fluor"))) == 0)
              assertMatrix(fpoints, mode = "numeric")
              private$.fpoints <- fpoints
            },
            .asXMLnodes = function(node.name) {
              apply(private$.fpoints,
                    1,
                    function(fpoints.row) {
                      sprintf("<mdp><tmp>%s</tmp><fluor>%s</fluor></mdp>",
                              fpoints.row["tmp"],
                              fpoints.row["fluor"])
                    }) %>% paste0(collapse = "")
            }
          ),
          private = list(
            .fpoints = NULL
          ),
          active = list(
            fpoints = function(fpoints) {
              if (missing(fpoints))
                return(private$.fpoints)
              assert(length(setdiff(colnames(fpoints), c("tmp", "fluor"))) == 0)
              assertMatrix(fpoints, mode = "numeric")
              private$.fpoints <- fpoints
            }
          ))

# dataType ------------------------------------------------------------

#' dataType R6 class.
#' 
#' Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{dataType$new(tar, cq = NULL, excl = NULL, 
#' adp = NULL, mdp = NULL, endPt = NULL, bgFluor = NULL, bgFluorSlp = NULL, 
#' quantFluor = NULL)}
#'   
#' @section Fields: \describe{ 
#' \item{\code{tar}}{\link{idReferencesType}. 
#'   TargetID - A reference to a target.} 
#'   \item{\code{cq}}{\link[checkmate]{checkNumber}. 
#'   Calculated fractional PCR cycle used for downstream quantification. 
#'   Negative values express following condition: Not Available: -1.0 } 
#'   \item{\code{excl}}{\link[checkmate]{checkString}. Excluded. If \code{excl}
#'   is present, this entry should not be evaluated. Do not set this element 
#'   to \code{FALSE} if the entry is valid. Instead, leave the entire \code{excl} 
#'   element out instead. It may contain a string with a reason for the exclusion. 
#'   Several reasons for exclusion should be 
#'   seperated by semicolons ";".} 
#'   \item{\code{adp}}{\link{adpsType}.} 
#'   \item{\code{mdp}}{\link{mdpsType}.} 
#'   \item{\code{endPt}}{\link[checkmate]{checkNumber}}. Value of the endpoint measurement. 
#'   \item{\code{bgFluor}}{\link[checkmate]{checkNumber}. Background 
#'   fluorescence (the y-intercept of the baseline trend based on the estimated
#'   background fluorescence). } 
#'   \item{\code{bgFluorSlp}}{\link[checkmate]{checkNumber}. 
#'   Background fluorescence slope - The slope of the baseline trend based on 
#'   the estimated background fluorescence. The element should be absent to 
#'   indicate a slope of 0.0; If this element is present without the \code{bgFluor} 
#'   element it should be ignored. } 
#'   \item{\code{quantFluor}}{\link[checkmate]{checkNumber}. Quantification flourescence -
#'   The fluorescence value corresponding to the treshold line.} }
#'   
#' @section Methods: \describe{
#' \item{\code{AsDataFrame(dp.type = "adp")}}{Represents amplification 
#' (\preformatted{dp.type = "adp"}) or melting (\code{dp.type = "mdp"}) data 
#' points as \code{data.frame}}
#' }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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
              assertClass(tar, "idReferencesType")
              assert(checkNull(cq),
                     checkNumber(cq))
              assert(checkNull(excl),
                     checkString(excl))
              assert(checkNull(adp),
                     checkClass(adp, "adpsType"))
              assert(checkNull(mdp),
                     checkClass(mdp, "mdpsType"))
              assert(checkNull(endPt),
                     checkNumber(endPt))
              assert(checkNull(bgFluor),
                     checkNumber(bgFluor))
              assert(checkNull(bgFluorSlp),
                     checkNumber(bgFluorSlp))
              assert(checkNull(quantFluor),
                     checkNumber(quantFluor))
              
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
              self[[dp.type]]$fpoints %>% 
                as.data.frame %>% 
                select(ifelse(dp.type == "adp",
                              cyc,
                              tmp),
                       fluor)
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
              assertClass(tar, "idReferencesType")
              private$.tar <- tar
            },
            cq = function(cq) {
              if (missing(cq))
                return(private$.cq)
              assert(checkNull(cq),
                     checkNumber(cq))
              private$.cq <- cq
            },
            excl = function(excl) {
              if (missing(excl))
                return(private$.excl)
              assert(checkNull(excl),
                     checkString(excl))
              private$.excl <- excl
            },
            adp = function(adp) {
              if (missing(adp))
                return(private$.adp)
              assert(checkNull(adp),
                     checkClass(adp, "adpsType"))
              private$.adp <- adp
            },
            mdp = function(mdp) {
              if (missing(mdp))
                return(private$.mdp)
              assert(checkNull(mdp),
                     checkClass(mdp, "mdpsType"))
              private$.mdp <- mdp
            },
            endPt = function(endPt) {
              if (missing(endPt))
                return(private$.endPt)
              assert(checkNull(endPt),
                     checkNumber(endPt))
              private$.endPt <- endPt
            },
            bgFluor = function(bgFluor) {
              if (missing(bgFluor))
                return(private$.bgFluor)
              assert(checkNull(bgFluor),
                     checkNumber(bgFluor))
              private$.bgFluor <- bgFluor
            },
            bgFluorSlp = function(bgFluorSlp) {
              if (missing(bgFluorSlp))
                return(private$.bgFluorSlp)
              assert(checkNull(bgFluorSlp),
                     checkNumber(bgFluorSlp))
              private$.bgFluorSlp <- bgFluorSlp
            },
            quantFluor = function(quantFluor) {
              if (missing(quantFluor))
                return(private$.quantFluor)
              assert(checkNull(quantFluor),
                     checkNumber(quantFluor))
              private$.quantFluor <- quantFluor
            }
          ))

# reactType ------------------------------------------------------------

#' reactType R6 class.
#' 
#' A reaction is an independent chemical reaction corresponding for example to a
#' well in a 96 well plate, a capillary in a rotor, a through-hole on an array, 
#' etc. Inherits: \link{rdmlBaseType}.
#' 
#' The ID of this reaction
#' 
#' Schemas : \itemize{ \item rotor : assign IDs according to the position of the
#' sample on the rotor (1 for the 1st sample, 2 for the 2nd, ...) \item plate 
#' (96/384/1536 well) : the IDs are assigned in a row-first/column-second 
#' manner. For each row, the samples are numbered according to the increasing 
#' column number. At the end of a row, the numbering starts at the first column 
#' of the next row. An example for this type of plate can be found below : 
#' \tabular{lllll}{ \tab 1  \tab 2  \tab 3 \tab ... \cr A   \tab 1  \tab 2  \tab
#' 3 \tab     \cr B   \tab 13 \tab 14 \tab   \tab     \cr ... \tab    \tab \tab 
#' \tab    } or \tabular{lllll}{ \tab 1  \tab 2  \tab 3 \tab ... \cr 1 \tab 1 
#' \tab 2  \tab 3 \tab     \cr 2   \tab 13 \tab 14 \tab   \tab     \cr ... \tab 
#' \tab    \tab   \tab    }
#' 
#' \item multi-array plate (BioTrove) : the IDs are assigned in a 
#' row-first/column-second manner, ignoring the organisation of sub-arrays. For 
#' each row, the samples are numbered according to the increasing column number.
#' At the end of a row, the the next row. An example for this type of plate can 
#' be found below : todo... }
#' 
#' @section Initialization: \preformatted{reactType$new(id, sample, data = NULL)}
#'   
#'   @section Fields: \describe{
#'   \item{\code{id}}{\link{reactIdType}. See 'Details'.} 
#'   \item{\code{sample}}{\link{idReferencesType}. SampleID - A reference to a
#'   sample.} 
#'   \item{\code{data}}{\code{list} of \link{dataType}.}
#'   }
#'   
#' @section Methods: \describe{\item{\code{AsDataFrame(dp.type = 
#'   "adp")}}{Represents amplification (\code{dp.type = "adp"}) or melting 
#'   (\code{dp.type = "mdp"}) data points of all targets as one 
#'   \code{data.frame}} \item{\code{Position(pcrformat)}}{Converts \code{react 
#'   id} to thew human readable form (i.e. '13' -> 'B1'). \code{pcrFormat} is 
#'   \code{pcrFormatType}. Only for 'ABC' '123' format!}}
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
reactType <- 
  R6Class("reactType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  sample,
                                  data = NULL) {
              assertClass(id, "reactIdType")
              assertClass(sample, "idReferencesType")
              assert(checkNull(data),
                     checkList(data, "dataType"))
              private$.id <- id
              private$.sample <- sample
              private$.data <- with.names(data,
                                          quote(.$tar$id))
            },
            AsDataFrame = function(dp.type = "adp",
                                   long.table = FALSE) {
              assertString(dp.type)
              out <-
                private$.data %>% 
                ldply(function(data)
                  data$AsDataFrame(dp.type) %>% 
                    cbind(tar = data$tar$id),
                  .id = "tar")
              
              if (long.table == FALSE) out <- out %>% spread(tar,
                                                             fluor) 
              out
            },
            Position = function(pcrFormat) {
              assertClass(pcrFormat, "pcrFormatType")
              stopifnot(pcrFormat$rowLabel$value == "ABC",
                        pcrFormat$columnLabel$value == "123")
              sprintf("%s%02i",
                      LETTERS[(private$.id$id - 1) %/% pcrFormat$columns + 1],
                      as.integer((private$.id$id - 1) %% pcrFormat$columns + 1))
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
              assertClass(id, "reactIdType")
              private$.id <- id
            },
            sample = function(sample) {
              if (missing(sample))
                return(private$.sample)
              assertClass(sample, "idReferencesType")
              private$.sample <- sample
            },
            data = function(data) {
              if (missing(data))
                return(private$.data)
              assert(checkNull(data),
                     checkList(data, "dataType"))
              private$.data <- with.names(data,
                                          quote(.$tar$id))
            }
          ))

# dataCollectionSoftwareType ------------------------------------------------------------

#' dataCollectionSoftwareType R6 class.
#' 
#' Software name and version used to collect and analyze the data. Inherits: 
#' \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{dataCollectionSoftwareType$new(name, version)}
#'
#'   @section Fields: \describe{   
#' \item{\code{name}}{\link[checkmate]{checkString}.}
#' \item{\code{version}}{\link[checkmate]{checkString}.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
#' @examples 
#' dataCollectionSoftwareType$new(name = "ExampleSoft", 
#'                                version = "1.0")
dataCollectionSoftwareType <- 
  R6Class("dataCollectionSoftwareType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(name,
                                  version) {
              assertString(name)
              assertString(version)
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
              assertString(name)
              private$.name <- name
            },
            version = function(version) {
              if (missing(version))
                return(private$.version)
              assertString(version)
              private$.version <- version
            }
          ))

# cqDetectionMethodType ------------------------------------------------------------

#' cqDetectionMethodType R6 class.
#' 
#' The method used to determine the Cq value.
#' Can take values:
#' \describe{
#' \item{"automated threshold and baseline settings"}{}
#' \item{"manual threshold and baseline settings"}{}
#' \item{"second derivative maximum"}{}
#' \item{"other"}{}
#' }  
#' Inherits: \link{enumType}.
#' 
#' @section Initialization: \preformatted{cqDetectionMethodType$new(value)}
#'   
#'   @section Fields: \describe{
#' \item{\code{value}}{\link[checkmate]{checkString}.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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

#' labelFormatType R6 class.
#' 
#' Label used for \link{pcrFormatType}.
#' Can take values:
#' \describe{
#' \item{ABC}{}
#' \item{123}{}
#' \item{A1a1}{}
#' }  
#' Inherits: \link{enumType}.
#' 
#' @section Initialization: \preformatted{labelFormatType$new(value)}
#'   
#'   @section Fields: \describe{
#' \item{\code{value}}{\link[checkmate]{checkString}.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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

#' pcrFormatType R6 class.
#' 
#' The display format of the PCR, analogous to the the qPCR instrument run format. 
#' Inherits: \link{rdmlBaseType}.
#' 
#' Rotor formats always have 1 column; rows correspond to the number of places 
#' in the rotor. Values for common formats are: \tabular{lllll}{ Format \tab
#' rows \tab columns \tab rowLabel \tab columnLabel \cr single-well \tab 1   
#' \tab 1       \tab 123      \tab 123         \cr 48-well plate \tab 6    \tab
#' 8       \tab ABC      \tab 123         \cr 96-well plate \tab 8    \tab 12   
#' \tab ABC      \tab 123         \cr 384-well plate \tab 16   \tab 24      \tab
#' ABC      \tab 123         \cr 1536-well plate \tab 32   \tab 48      \tab ABC
#' \tab 123         \cr 3072-well array \tab 32   \tab 96      \tab A1a1    
#' \tab A1a1        \cr 5184-well chip \tab 72   \tab 72      \tab ABC      \tab
#' 123         \cr 32-well rotor \tab 32   \tab 1       \tab 123      \tab 123  
#' \cr 72-well rotor \tab 72   \tab 1       \tab 123      \tab 123         \cr
#' 100-well rotor \tab 100  \tab 1       \tab 123      \tab 123         \cr free
#' format \tab -1   \tab 1       \tab 123      \tab 123 } If rows field has value -1,
#' the function will not try to reconstruct a plate and just display all run
#' data in a single column. If the columns field has value 1 then the function will 
#' not display a column label.
#' 
#' @section Initialization: \preformatted{pcrFormatType$new(rows, columns, rowLabel, columnLabel)}
#'   
#'   @section Fields: \describe{
#' \item{\code{rows}}{\link[checkmate]{checkCount}.}
#' \item{\code{columns}}{\link[checkmate]{checkCount}.}
#' \item{\code{rowLabel}}{\link{labelFormatType}.}
#' \item{\code{columnLabel}}{\link{labelFormatType}.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
pcrFormatType <- 
  R6Class("pcrFormatType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(rows,
                                  columns,
                                  rowLabel,
                                  columnLabel) {
              assertCount(rows)
              assertCount(columns)
              assertClass(rowLabel, "labelFormatType")
              assertClass(columnLabel, "labelFormatType")
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
              assertCount(rows)
              private$.rows <- rows
            },
            columns = function(columns) {
              if (missing(columns))
                return(private$.columns)
              assertCount(columns)
              private$.columns <- columns
            },
            rowLabel = function(rowLabel) {
              if (missing(rowLabel))
                return(private$.rowLabel)
              assertClass(rowLabel, "labelFormatType")
              private$.rowLabel <- rowLabel
            },
            columnLabel = function(columnLabel) {
              if (missing(columnLabel))
                return(private$.columnLabel)
              assertClass(columnLabel, "labelFormatType")
              private$.columnLabel <- columnLabel
            }
          ))

# runType ------------------------------------------------------------

#' runType R6 class.
#' 
#' A run is a set of reactions performed in one "run", for example one plate, 
#' one rotor, one array, one chip. Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{runType$new(id, description = NULL, 
#'   documentation = NULL, experimenter = NULL, instrument = NULL, 
#'   dataCollectionSoftware = NULL, backgroundDeterminationMethod = NULL, 
#'   cqDetectionMethod = NULL, thermalCyclingConditions = NULL, pcrFormat, 
#'   runDate = NULL, react = NULL)}
#'   
#' @section Fields: \describe{ 
#'  \item{\code{id}}{\link{idType}.} 
#'  \item{\code{description}}{\link[checkmate]{checkString}.} 
#'  \item{\code{documentation}}{\code{list} of \link{idReferencesType}.} 
#'  \item{\code{experimenter}}{\code{list} of \link{idReferencesType}.} 
#'  \item{\code{instrument}}{\link[checkmate]{checkString}. Description of the 
#'   instrument used to aquire the data.} 
#'  \item{\code{dataCollectionSoftware}}{\link{dataCollectionSoftwareType}. 
#'   Description of the software used to analyze/collect the data.} 
#'  \item{\code{backgroundDeterminationMethod}}{\link[checkmate]{checkString}. 
#'   Description of method used to determine the background. } 
#'  \item{\code{cqDetectionMethod}}{\link{cqDetectionMethodType}. Description of method 
#'   used to calculate the quantification cycle. } 
#'  \item{\code{thermalCyclingConditions}}{\link{idReferencesType}. The program 
#'   used to aquire the data.} 
#'  \item{\code{pcrFormat}}{\link{adpsType}.} 
#'  \item{\code{runDate}}{\link{adpsType}. Time stamp of data acquisition.} 
#'  \item{\code{react}}{\code{list} of \link{adpsType}.} }
#'   
#' @section Methods: \describe{
#'  \item{\code{AsDataFrame(dp.type = "adp")}}{Represents amplification 
#'  (\code{dp.type = "adp"}) or melting (\code{dp.type = "mdp"}) data 
#'  points as \code{data.frame}}}
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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
              assertClass(id, "idType")
              assert(checkNull(description),
                     checkString(description))
              assert(checkNull(documentation),
                     checkList(documentation, "idReferencesType"))
              assert(checkNull(experimenter),
                     checkList(experimenter, "idReferencesType"))
              assert(checkNull(instrument),
                     checkString(instrument))
              assert(checkNull(dataCollectionSoftware),
                     checkClass(dataCollectionSoftware, "dataCollectionSoftwareType"))
              assert(checkNull(backgroundDeterminationMethod),
                     checkString(backgroundDeterminationMethod))
              assert(checkNull(cqDetectionMethod),
                     checkClass(cqDetectionMethod, "cqDetectionMethodType"))
              assert(checkNull(thermalCyclingConditions),
                     checkClass(thermalCyclingConditions, "idReferencesType"))
              assertClass(pcrFormat, "pcrFormatType")
              assert(checkNull(runDate),
                     checkString(runDate)) # date time
              assert(checkNull(react),
                     checkList(react, "reactType"))
              
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
              private$.react <- with.names(react,
                                           quote(.$id$id))
            },
            AsDataFrame = function(dp.type = "adp",
                                   long.table = FALSE) {
              assertString(dp.type)
              out <- 
                private$.react %>% 
                ldply(function(react)
                  
                  react$AsDataFrame(
                    dp.type = dp.type,
                    long.table = TRUE) %>% 
                    cbind(.,
                          sname = 
                            sprintf("%s_%s",
                                    react$id$id,
                                    react$sample$id)
                    ),
                  .id = ifelse(dp.type == "adp",
                               "cyc",
                               "tmp"))
              
              if (long.table == FALSE) out <- out %>% 
                tidyr::unite(sname_tar, sname, tar, sep = "_") %>% 
                tidyr::spread(sname_tar, fluor) #%>% 
              #                   arrange(ifelse(dp.type == "adp",
              #                                  cyc,
              #                                  tmp))
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
              assertClass(id, "idType")
              private$.id <- id
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert(checkNull(description),
                     checkString(description))
              private$.description <- description
            },
            documentation = function(documentation) {
              if (missing(documentation))
                return(private$.documentation)
              assert(checkNull(documentation),
                     checkList(documentation, "idReferencesType"))
              private$.documentation <- documentation
            },
            experimenter = function(experimenter) {
              if (missing(experimenter))
                return(private$.experimenter)
              assert(checkNull(experimenter),
                     checkList(experimenter, "idReferencesType"))
              private$.experimenter <- experimenter
            },
            instrument = function(instrument) {
              if (missing(instrument))
                return(private$.instrument)
              assert(checkNull(instrument),
                     checkString(instrument))
              private$.instrument <- instrument
            },
            dataCollectionSoftware = function(dataCollectionSoftware) {
              if (missing(dataCollectionSoftware))
                return(private$.dataCollectionSoftware)
              assert(checkNull(dataCollectionSoftware),
                     checkClass(dataCollectionSoftware, "dataCollectionSoftwareType"))
              private$.dataCollectionSoftware <- dataCollectionSoftware
            },
            backgroundDeterminationMethod = function(backgroundDeterminationMethod) {
              if (missing(backgroundDeterminationMethod))
                return(private$.backgroundDeterminationMethod)
              assert(checkNull(backgroundDeterminationMethod),
                     checkString(backgroundDeterminationMethod))
              private$.backgroundDeterminationMethod <- backgroundDeterminationMethod
            },
            cqDetectionMethod = function(cqDetectionMethod) {
              if (missing(cqDetectionMethod))
                return(private$.cqDetectionMethod)
              assert(checkNull(cqDetectionMethod),
                     checkClass(cqDetectionMethod, "cqDetectionMethodType"))
              private$.cqDetectionMethod <- cqDetectionMethod
            },
            thermalCyclingConditions = function(thermalCyclingConditions) {
              if (missing(thermalCyclingConditions))
                return(private$.thermalCyclingConditions)
              assert(checkNull(thermalCyclingConditions),
                     checkClass(thermalCyclingConditions, "idReferencesType"))
              private$.thermalCyclingConditions <- thermalCyclingConditions
            },
            pcrFormat = function(pcrFormat) {
              if (missing(pcrFormat))
                return(private$.pcrFormat)
              assertClass(pcrFormat, "pcrFormatType")
              private$.pcrFormat <- pcrFormat
            },
            runDate = function(runDate) {
              if (missing(runDate))
                return(private$.runDate)
              assert(checkNull(runDate),
                     checkString(runDate)) # date time
              private$.runDate <- runDate
            },
            react = function(react) {
              if (missing(react))
                return(private$.react)
              assert(checkNull(react),
                     checkList(react, "reactType"))
              private$.react <- with.names(react,
                                           quote(.$id$id))
            }
          ))

# experimentType ------------------------------------------------------------

#' experimentType R6 class.
#' 
#' A qPCR experiment. It may contain several runs (\link{runType}). Inherits: 
#' \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{experimentType$new(id, description = NULL,
#'   documentation = NULL, run = NULL)}
#'
#'   @section Fields: \describe{   
#' \item{\code{id}}{\link{idType}.}
#' \item{\code{description}}{\link[checkmate]{checkString}.}
#' \item{\code{documentation}}{\code{list} of \link{idReferencesType}.}
#' \item{\code{run}}{\code{list} of \link{runType}.}
#' }
#'   
#' @section Methods: \describe{\item{\code{AsDataFrame(dp.type = "adp", 
#'   long.table = FALSE)}}{Represents amplification (\code{dp.type = "adp"}) or 
#'   melting (\code{dp.type = "mdp"}) data points as \code{data.frame}. 
#'   \code{long.table = TRUE} means that fluorescence data for all runs and 
#'   reacts will be at one collumn.}}
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
experimentType <- 
  R6Class("experimentType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(id,
                                  description = NULL,
                                  documentation = NULL,
                                  run = NULL) {
              assertClass(id, "idType")
              assert(checkNull(description),
                     checkString(description))
              assert(checkNull(documentation),
                     checkList(documentation, "idReferencesType"))
              assert(checkNull(run),
                     checkList(run, "runType"))
              
              private$.id <- id
              private$.description <- description
              private$.documentation <- documentation
              private$.run <- with.names(run,
                                         quote(.$id$id))
            },
            AsDataFrame = function(dp.type = "adp",
                                   long.table = FALSE) {
              assertString(dp.type)
              assertFlag(long.table)
              out <-
                private$.run %>% 
                ldply(function(run)
                  run$AsDataFrame(
                    dp.type = dp.type,
                    long.table = TRUE) %>% 
                    cbind(.,  run = run$id$id,
                          row.names = NULL),
                  .id = ifelse(dp.type == "adp",
                               "cyc",
                               "tmp"))
              if (long.table == FALSE) out <- out %>%
                tidyr::unite(run_sname_tar, run, sname, tar, sep = "_") %>% 
                tidyr::spread(run_sname_tar, fluor)
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
              assertClass(id, "idType")
              private$.id <- id
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert(checkNull(description),
                     checkString(description))
              private$.description <- description
            },
            documentation = function(documentation) {
              if (missing(documentation))
                return(private$.documentation)
              assert(checkNull(documentation),
                     checkList(documentation, "idReferencesType"))
              private$.documentation <- documentation
            },
            run = function(run) {
              if (missing(run))
                return(private$.run)
              assert(checkNull(run),
                     checkList(run, "runType"))
              private$.run <- with.names(run,
                                         quote(.$id$id))
            }
          ))

# lidOpenType ------------------------------------------------------------

#' lidOpenType R6 class.
#' 
#' This step waits for the user to open the lid and continues afterwards. It 
#' allows to stop the program and to wait for the user to add for example 
#' enzymes and continue the program afterwards. The temperature of the previous 
#' step is maintained. Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{lidOpenType$new()}
#'   
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
lidOpenType <- 
  R6Class("lidOpenType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function() {
            }
          ),
          private = list(
          ))

# pauseType ------------------------------------------------------------

#' pauseType R6 class.
#' 
#' This step allows to pause at a certain temperature. It is typically the last 
#' step in an amplification protocol. Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{pauseType$new(temperature)}
#'   
#' @section Fields: \describe{ \item{\code{temperature}}{\link[checkmate]{checkNumber}.
#'   The temperature in degrees Celsius maintained during the pause.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
pauseType <- 
  R6Class("pauseType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(temperature) {
              assertNumber(temperature)
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
              assertNumber(temperature)
              private$.temperature <- temperature
            }
          ))


# loopType ------------------------------------------------------------

#' loopType R6 class.
#' 
#' This step allows to form a loop or to exclude some steps. It allows to jump 
#' to a certain "goto" step for "repeat" times. If the "goto" step is outside of 
#' the loop range, it must have "repeat" value "0". Inherits: 
#' \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{loopType$new(goto, repeat.n)}
#'   
#' @section Fields: \describe{ \item{\code{goto}}{\link[checkmate]{assertCount}.  The
#'   step to go to to form the loop.}
#' \item{\code{repeat.n}}{\link[checkmate]{assertCount}. Determines how many times the loop is 
#'   repeated. The first run through the loop is counted as 0, the last loop is 
#'   "repeat" - 1.}}
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
loopType <- 
  R6Class("loopType",
          # class = FALSE,
          inherit = rdmlBaseType,
          public = list(
            initialize = function(goto,
                                  repeat.n) {
              assertCount(goto)
              assertCount(repeat.n)
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
              assertCount(goto)
              private$.goto <- goto
            },
            repeat.n = function(repeat.n) {
              if (missing(repeat.n))
                return(private$.repeat)
              assertCount(repeat.n)
              private$.repeat <- repeat.n
            }
          ))

# measureType ------------------------------------------------------------

#' measureType R6 class.
#' 
#' Can take values:
#' \describe{
#' \item{real time}{}
#' \item{meltcurve}{}
#' }  
#' Inherits: \link{enumType}.
#' 
#' @section Initialization: \preformatted{measureType$new(value)}
#' 
#'   @section Fields: \describe{ 
#' \item{\code{value}}{\link[checkmate]{checkString}.}
#' }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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

#' baseTemperatureType R6 class.
#' 
#' Parent class for inner usage. Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{baseTemperatureType$new(duration, 
#'   temperatureChange = NULL, durationChange = NULL, measure = NULL, ramp = 
#'   NULL)}
#'   
#' @section Fields: \describe{ 
#'   \item{\code{duration}}{\link[checkmate]{checkCount}. Duration of this
#'   step in seconds.}
#'   \item{\code{temperatureChange}}{\link[checkmate]{checkNumber}. Change 
#'   of the temperature between two consecutive cycles: actual temperature
#'   = temperature + (temperatureChange * cycle counter)}
#'   \item{\code{durationChange}}{\link[checkmate]{checkCount}. Change of the
#'   duration between two consecutive cycles: actual duration = duration +
#'   (durationChange * cycle counter)}
#'   \item{\code{measure}}{\link{measureType}. Indicates to make a measurement 
#'   and store it as meltcurve or real-time data.}
#'   \item{\code{ramp}}{\link[checkmate]{checkNumber}. Allowed temperature
#'   change between two consecutive cycles in degrees Celsius per second. If unstated,
#'   the maximal change rate is assumed.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
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
              assertCount(duration)
              assert(checkNull(temperatureChange),
                     checkNumber(temperatureChange))
              assert(checkNull(durationChange),
                     checkCount(durationChange))
              assert(checkNull(measure),
                     checkClass(measure, "measureType"))
              assert(checkNull(ramp),
                     checkNumber(ramp))
              
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
              assertCount(duration)
              private$.duration <- duration
            },
            temperatureChange = function(temperatureChange) {
              if (missing(temperatureChange))
                return(private$.temperatureChange)
              assert(checkNull(temperatureChange),
                     checkNumber(temperatureChange))
              private$.temperatureChange <- temperatureChange
            },
            durationChange = function(durationChange) {
              if (missing(durationChange))
                return(private$.durationChange)
              assert(checkNull(durationChange),
                     checkCount(durationChange))
              private$.durationChange <- durationChange
            },
            measure = function(measure) {
              if (missing(measure))
                return(private$.measure)
              assert(checkNull(measure),
                     checkClass(measure, "measureType"))
              private$.measure <- measure
            },
            ramp = function(ramp) {
              if (missing(ramp))
                return(private$.ramp)
              assert(checkNull(ramp),
                     checkNumber(ramp))
              private$.ramp <- ramp
            }
          ))

# temperatureType ------------------------------------------------------------

#' temperatureType R6 class.
#' 
#' This step keeps a constant temperature on the heat block. Inherits: 
#' \link{baseTemperatureType}.
#' 
#' @section Initialization: \preformatted{temperatureType$new(temperature, ...)}
#'   
#' @section Fields: \describe{ \item{\code{temperature}}{\link[checkmate]{checkNumber}.
#'   The temperature of the step in  degrees Celsius.}
#' \item{\code{...}}{ Params of parent class \link{baseTemperatureType}.}
#' } 
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
temperatureType <- 
  R6Class("temperatureType",
          # class = FALSE,
          inherit = baseTemperatureType,
          public = list(
            initialize = function(temperature, ...) {
              assertNumber(temperature)
              private$.temperature <- temperature
              super$initialize(...)
            }
          ),
          private = list(
            .temperature = NULL
          ),
          active = list(
            temperature = function(temperature) {
              if (missing(temperature))
                return(private$.temperature)
              assertNumber(temperature)
              private$.temperature <- temperature
            }
          ))

# gradientType ------------------------------------------------------------

#' gradientType R6 class.
#' 
#' Details of the temperature gradient across the PCR block. Inherits: 
#' \link{baseTemperatureType}.
#' 
#' @section Initialization: \preformatted{gradientType$new(highTemperature, 
#'   lowTemperature, ...)}
#'   
#' @section Fields: \describe{ 
#'   \item{\code{highTemperature}}{\link[checkmate]{checkNumber}. The highest temperature of
#'   the gradient in degrees Celsius.}
#'   \item{\code{lowTemperature}}{\link[checkmate]{checkNumber}. The lowest temperature of
#'   the gradient in degrees Celsius.}
#' \item{\code{...}}{ Params of parent class \link{baseTemperatureType}. }}
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
gradientType <- 
  R6Class("gradientType",
          # class = FALSE,
          inherit = baseTemperatureType,
          public = list(
            initialize = function(highTemperature,
                                  lowTemperature,
                                  ...) {
              assertNumber(highTemperature)
              assertNumber(lowTemperature)
              private$.highTemperature <- highTemperature
              private$.lowTemperature <- lowTemperature
              super$initialize(...)
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
              assertNumber(highTemperature)
              private$.highTemperature <- highTemperature
            },
            lowTemperature = function(lowTemperature) {
              if (missing(lowTemperature))
                return(private$.lowTemperature)
              assertNumber(lowTemperature)
              private$.lowTemperature <- lowTemperature
            }
          ))


# stepType ------------------------------------------------------------

#' stepType R6 class.
#' 
#' Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{stepType$new(nr, description = NULL, 
#'   temperature = NULL, gradient = NULL, loop = NULL, pause = NULL, lidOpen = 
#'   NULL)}
#'   
#' @section Fields: \describe{ \item{\code{nr}}{\link[checkmate]{checkCount}. The
#'   incremental number of the step. First step should have value 1. The increment 
#'   between steps should be constant and equivalent to 1.}
#'   \item{\code{description}}{\link[checkmate]{checkString}.} 
#'   \item{\code{temperature}}{\link{temperatureType}.} 
#'   \item{\code{gradient}}{\link{gradientType}.} 
#'   \item{\code{loop}}{\link{loopType}.} \item{\code{pause}}{\link{pauseType}.}
#'   \item{\code{lidOpen}}{\link{lidOpenType}.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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
              assertCount(nr)
              assert(checkNull(description),
                     checkString(description))
              assert(checkNull(temperature),
                     checkClass(temperature, "temperatureType"))
              assert(checkNull(gradient),
                     checkClass(gradient, "gradientType"))
              assert(checkNull(loop),
                     checkClass(loop, "loopType"))
              assert(checkNull(pause),
                     checkClass(pause, "pauseType"))
              assert(checkNull(lidOpen),
                     checkClass(lidOpen, "lidOpenType"))
              
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
              assertCount(nr)
              private$.nr <- nr
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert(checkNull(description),
                     checkString(description))
              private$.description <- description
            },
            temperature = function(temperature) {
              if (missing(temperature))
                return(private$.temperature)
              assert(checkNull(temperature),
                     checkClass(temperature, "temperatureType"))
              private$.temperature <- temperature
            },
            gradient = function(gradient) {
              if (missing(gradient))
                return(private$.gradient)
              assert(checkNull(gradient),
                     checkClass(gradient, "gradientType"))
              private$.gradient <- gradient
            },
            loop = function(loop) {
              if (missing(loop))
                return(private$.loop)
              assert(checkNull(loop),
                     checkClass(loop, "loopType"))
              private$.loop <- loop
            },
            pause = function(pause) {
              if (missing(pause))
                return(private$.pause)
              assert(checkNull(pause),
                     checkClass(pause, "pauseType"))
              private$.pause <- pause
            },
            lidOpen = function(lidOpen) {
              if (missing(lidOpen))
                return(private$.lidOpen)
              assert(checkNull(lidOpen),
                     checkClass(lidOpen, "lidOpenType"))
              private$.lidOpen <- lidOpen
            }
          ))

# thermalCyclingConditionsType ------------------------------------------------------------

#' thermalCyclingConditionsType R6 class.
#' 
#' A cycling program for PCR or to amplify cDNA. Inherits: \link{rdmlBaseType}.
#' 
#' @section Initialization: \preformatted{thermalCyclingConditionsType$new(id, 
#'   description = NULL, documentation = NULL, lidTemperature = NULL, 
#'   experimenter = NULL, step)}
#'   
#' @section Fields: \describe{ \item{\code{id}}{\link{idType}.} 
#'   \item{\code{description}}{\link[checkmate]{checkString}.} 
#'   \item{\code{documentation}}{\code{list} of \link{idReferencesType}.} 
#'   \item{\code{lidTemperature}}{\link[checkmate]{checkNumber}. The temperature in
#'   degrees Celsius of the lid during cycling. }
#'   \item{\code{experimenter}}{\code{list} of \link{idReferencesType}.
#'   Reference to the person who made or uses this protocol. }
#'   \item{\code{step}}{\code{list} of \link{stepType}. The steps a protocol
#'   runs through to amplify DNA.}
#'   }
#'   
#' @docType class
#' @format An \code{\link{R6Class}} generator object.
#' @export
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
              assertClass(id, "idType")
              assert(checkNull(description),
                     checkString(description))
              assert(checkNull(documentation),
                     checkList(documentation, "idReferencesType"))
              assert(checkNull(lidTemperature),
                     checkNumber(lidTemperature))
              assert(checkNull(experimenter),
                     checkList(experimenter, "idReferencesType"))
              assert(checkNull(step),
                     checkList(step, "stepType"))
              
              private$.id <- id
              private$.description <- description
              private$.documentation <- documentation
              private$.lidTemperature <- lidTemperature
              private$.experimenter <- experimenter
              private$.step <- with.names(step,
                                          quote(.$nr))
              
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
              assertClass(id, "idType")
              private$.id <- id
            },
            description = function(description) {
              if (missing(description))
                return(private$.description)
              assert(checkNull(description),
                     checkString(description))
              private$.description <- description
            },
            documentation = function(documentation) {
              if (missing(documentation))
                return(private$.documentation)
              assert(checkNull(documentation),
                     checkList(documentation, "idReferencesType"))
              private$.documentation <- documentation
            },
            lidTemperature = function(lidTemperature) {
              if (missing(lidTemperature))
                return(private$.lidTemperature)
              assert(checkNull(lidTemperature),
                     checkNumber(lidTemperature))
              private$.lidTemperature <- lidTemperature
            },
            experimenter = function(experimenter) {
              if (missing(experimenter))
                return(private$.experimenter)
              assert(checkNull(experimenter),
                     checkList(experimenter, "idReferencesType"))
              private$.experimenter <- experimenter
            },
            step = function(step) {
              if (missing(step))
                return(private$.step)
              assert(checkNull(step),
                     checkList(step, "stepType"))
              private$.step <- with.names(step,
                                          quote(.$nr))
            }
          ))
