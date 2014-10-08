# Unzips RDML to get inner XML content
GetRDMLdoc <- function(file)
{  
  unzipped.rdml <- unzip(file)
  tryCatch({
    if(length(unzipped.rdml) > 1)
    {
      rdml.doc <- xmlParse("rdml_data.xml")
    }
    else
    {
      rdml.doc <- xmlParse(unzipped.rdml)
    }},
    error = function(e) { print(e) },
    finally = unlink(unzipped.rdml))
  return(rdml.doc)
}

# Gets file publisher (instrumant manufacturer)
GetPublisher <- function(rdml.doc)
{
  publisher <- xpathSApply(
    rdml.doc, 
    "/rdml:rdml/rdml:id/rdml:publisher",
    xmlValue,
    namespaces = c(rdml = "http://www.rdml.org"))  
  if(length(publisher) != 0){ return(publisher) }
  else { return("StepOne") }
}

# Gets PCR samples descriptions vector from XML
GetDescriptions <- function(RDMLdoc)
{
  samplesids <- xpathSApply(
    RDMLdoc, 
    "/rdml:rdml/rdml:sample",
    xmlGetAttr,
    name = "id",
    namespaces = c(rdml = "http://www.rdml.org"))
  descriptions <- xpathSApply(
    RDMLdoc, 
    "/rdml:rdml/rdml:sample/rdml:description",
    xmlValue,
    namespaces = c(rdml = "http://www.rdml.org"))  
  names(descriptions) <- samplesids
  return(descriptions)
}

# Gets plate dimensions from XML
GetPlateDimensions <- function(rdml.doc)
{
  
  rows <- tryCatch({as.integer(xpathSApply(
    rdml.doc, 
    "/rdml:rdml/rdml:experiment/rdml:run/rdml:pcrFormat/rdml:rows",
    xmlValue,
    namespaces = c(rdml = "http://www.rdml.org"))[1])},
    error = function(e) 8)
  columns <- tryCatch({as.integer(xpathSApply(
    rdml.doc, 
    "/rdml:rdml/rdml:experiment/rdml:run/rdml:pcrFormat/rdml:columns",
    xmlValue,
    namespaces = c(rdml = "http://www.rdml.org"))[1])},
    error = function(e) 12)
  return(c(rows = rows, columns = columns))
}

# Getstype (Unknown, Positive, Standart, etc.)
# of each sample from XML
GetSamplesTypes <- function(rdml.doc)
{    
  samples.ids <- xpathSApply(
    rdml.doc, 
    "/rdml:rdml/rdml:sample",
    xmlGetAttr,
    name = "id",
    namespaces = c(rdml = "http://www.rdml.org"))
  samples.types <- xpathSApply(
    rdml.doc, 
    "/rdml:rdml/rdml:sample/rdml:type",
    xmlValue,
    namespaces = c(rdml = "http://www.rdml.org"))  
  names(samples.types) <- samples.ids
  return(samples.types)
}

# Gets concentrations (quantity) of each 
# dilution from XML
GetDilutions <- function(rdml.doc)
{
  # XML nodes that contain "quantity" information
  nodes <- getNodeSet(
    rdml.doc, 
    "/rdml:rdml/rdml:sample/rdml:quantity/rdml:value/../..",    
    namespaces = c(rdml = "http://www.rdml.org"))  
  if(is.null(nodes)) return()
  values <- sapply(nodes, function(node) {
    as.numeric(xmlValue(node[["quantity"]][["value"]]))})
  # names of the samples that contain "quantity" information
  samplesids <- sapply(nodes, xmlGetAttr, name = "id")
  names(values) <- samplesids
  # sorting quantities by sample name
  values <- values[order(names(values))]
  dils<- list()
  dils[[1]] <- values
  names(dils) <- "All Targets"
  return(dils)  
} 

# Get dilutions Roche
GetDilutionsRoche <- function(file)
{
  unzipped.rdml <- unzip(file)
  tryCatch({
    rdml.doc <- xmlParse("calculated_data.xml")
    unlink(unzipped.rdml) 
    
    nodes<- getNodeSet(
      rdml.doc, 
      "//ns:absQuantDataSource/ns:standard/..",    
      namespaces = c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"))
    
    dilutions <- list()
    for(node in nodes){
      quant <- xmlValue(node[["standard"]])
      position <- xpathSApply(
        rdml.doc, 
        paste0("//ns:standardPoints/ns:standardPoint/ns:graphIds/ns:guid[text() ='",
               xmlValue(node[["graphId"]]), "']/../../ns:position"), xmlValue,
        namespaces = c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"))
      dye <- xpathSApply(
        rdml.doc, 
        paste0("//ns:standardPoints/ns:standardPoint/ns:graphIds/ns:guid[text() ='",
               xmlValue(node[["graphId"]]), "']/../../ns:dyeName"), xmlValue,
        namespaces = c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"))
      dilutions[[dye]] <- cbind(dilutions[[dye]],c(position, quant))
    }
    dilutions <- lapply(dilutions, function(dilution){
      quant <- as.numeric(dilution[2, ])
      quant <- t(as.data.frame(quant))
      colnames(quant) <- dilution[1, ]
      return(quant) })
  },
  error = function(e) { print(e) },
  finally = unlink(unzipped.rdml))
  return(dilutions)
}

# Generates sample name by specified pattern
GenSName <- function (name.pattern,
                                plate.dims,
                                react.id,
                                tube.name,
                                tube,
                                target,
                                type,
                                publisher)
{ 
  name.pattern <- gsub("%NAME%",
                       tube.name,
                       name.pattern)
  name.pattern <- gsub("%ID%",
                       react.id,
                       name.pattern)
  name.pattern <- gsub("%TUBE%",
                       tube,
                       name.pattern)
  name.pattern <- gsub("%TARGET%",
                       target,
                       name.pattern)
  name.pattern <- gsub("%TYPE%",
                       type,
                       name.pattern)
  return(name.pattern)
}

RDMLnew <- R6Class("RDML",
                public = list(                
                  plate.name = NA,
                  publisher = NA,
                  dilutions = NA,
                  plate.dims = NA,
                  initialize = function(file.name,
                                        name.pattern = "%NAME%__%TUBE%") {
                    rdml.doc <- GetRDMLdoc(file.name)
                    self$publisher <- GetPublisher(rdml.doc)
                    if(self$publisher == "Roche Diagnostics") 
                      self$dilutions <- GetDilutionsRoche(file.name)                    
                    else self$dilutions <- GetDilutions(rdml.doc)
                    types <- GetSamplesTypes(rdml.doc)
                    self$plate.dims <- GetPlateDimensions(rdml.doc) 
                    if(self$publisher == "Roche Diagnostics")
                      sdescs <- GetDescriptions(rdml.doc)
                    
                    reacts <- getNodeSet(
                      rdml.doc,
                      "/rdml:rdml/rdml:experiment/rdml:run/rdml:react",
                      namespaces = c(rdml = "http://www.rdml.org"))
                    n.reacts = length(reacts)
                    n.qpcr.cycles <- length(reacts[[1]][["data"]]["adp"])
                    temp.stages <- sapply(reacts[[1]][["data"]]["mdp"], 
                                          function(mdp) {xmlValue(mdp[["tmp"]])})
                    n.temp.stages <- length(temp.stages)
                    
                    if(!is.na(n.qpcr.cycles)) {
                      private$qPCR.fdata <- matrix(nrow = n.qpcr.cycles,
                                            ncol = n.reacts,
                                            dimnames = list(row = 1:n.qpcr.cycles))
                    }
                    if(!is.na(n.temp.stages)) {
                      private$melt.fdata <- matrix(nrow = n.temp.stages,
                                            ncol = n.reacts,
                                            dimnames = list(row = temp.stages))
                    }
                    private$plate.map <- matrix(nrow = 5,
                                        ncol = n.reacts,
                                        dimnames = list(row = c("ReactID",
                                                                "Tube",
                                                                "TubeName",
                                                                "Target",
                                                                "Type"
                                                                )
                                                        ))
                    
                    for(react.i in 1:n.reacts) {                      
                      react.id <- xmlGetAttr(reacts[[react.i]], name = "id")
                      sample.id <- xmlGetAttr(reacts[[react.i]][["sample"]], name = "id")
                      tube.name <- ifelse(self$publisher == "Roche Diagnostics",
                                         sdescs[sample.id],
                                         sample.id)    
                      type <- types[sample.id]
                      # omit empty Bio-Rad data
#                       try({
                        for(fdata in reacts[[react.i]]["data", all = TRUE])
                        {
                          tube <- ifelse((
                            self$publisher == "StepOne"),
                            react.id,
                            {
                              react.id <- as.integer(react.id)
                              paste0(LETTERS[react.id %/% 
                                               self$plate.dims["columns"] + 1],
                                     react.id %% self$plate.dims["columns"])
                            }
                          )                            
                          target.id <- xmlGetAttr(fdata[["tar"]], name = "id")
                          if(target.id == "") target.id <- "NA"
                          adps <- sapply(fdata["adp", all = TRUE],
                                         function(x) as.numeric(xmlValue(x[["fluor"]])))
                          mdps <- sapply(fdata["mdp", all = TRUE],
                                         function(x) as.numeric(xmlValue(x[["fluor"]])))                            
                          # add qPCR data                          
                          if(length(adps) != 0) private$qPCR.fdata[,react.i] <- adps
                          # add melting data
                          if(length(mdps) != 0) private$melt.fdata[,react.i] <- mdps
                          
                          private$plate.map[,react.i] <- c(react.id,
                                                           tube,
                                                           tube.name,
                                                           target.id,
                                                           type)
                        }
#                       }
#                       , silent = TRUE)
                    }
                  },
                  GetFData = function(i) {
                    print(private$plate.map[,i])
                    print(private$qPCR.fdata[,i])
                    print(private$melt.fdata[,i])
                  }
                ),
                private = list(
                  qPCR.fdata = NA,                  
                  melt.fdata = NA,
                  plate.map = NA
                ),
                active = list(
                  targets = function() { NULL
                    
                  },
                  types = function(target = NA) { NULL
                    
                  }                  
                )
)
              