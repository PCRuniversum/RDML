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
  samples.ids <- xpathSApply(
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
  names(descriptions) <- samples.ids
  return(descriptions)
}

# Gets plate dimensions from XML
GetPlateDimensions <- function(rdml.doc)
{
  # delete trycatch, better to stop execution on error
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


GetDilutionsRoche <- function()
{
#   unzipped.rdml <- unzip(file)
  tryCatch({
    rdml.doc <- xmlParse("calculated_data.xml")
    concs<-as.numeric(xpathSApply(
      rdml.doc,
      "//ns:absQuantDataSource/ns:standard",   
      namespaces = c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"),
      xmlValue))
    concs.guids<-xpathSApply(
      rdml.doc,
      "//ns:absQuantDataSource/ns:standard/../ns:graphId",   
      namespaces = c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"),
      xmlValue)
    names(concs) <- concs.guids
    concs <- sort(concs, decreasing=TRUE)
    positions <- xpathSApply(
      rdml.doc, 
      paste0("//ns:standardPoints/ns:standardPoint/ns:position"), 
      xmlValue,
      namespaces = c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"))
    dye.names <- position <- xpathSApply(
      rdml.doc, 
      paste0("//ns:standardPoints/ns:standardPoint/ns:dyeName"), 
      xmlValue,
      namespaces = c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"))
    positions.guids <- position <- xpathSApply(
      rdml.doc, 
      paste0("//ns:standardPoints/ns:standardPoint/ns:graphIds/ns:guid"), 
      xmlValue,
      namespaces = c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"))
    positions.table <- matrix(c(dye.names,
                                positions),
                              ncol = length(positions),
                              nrow = 2,
                              byrow = TRUE,
                              dimnames = list(c("dye.name","position"),
                                              positions.guids))
    positions.table <- positions.table[,
      order(match(colnames(positions.table), names(concs)))]
    positions.table <- rbind(positions.table, conc = concs)
    dyes <- unique(positions.table["dye.name",])
    dilutions <- lapply(dyes, function(dye) {
      dye.group.indecies <- which(positions.table["dye.name",] == dye)
      concs.by.dye <- concs[dye.group.indecies]
      names(concs.by.dye) <- positions.table["position",
                                             dye.group.indecies]
      concs.by.dye
    })
    names(dilutions) <- dyes
  },
  error = function(e) { print(e) }#,
#   finally = unlink(unzipped.rdml)
  )
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
                       # Unzips RDML to get inner XML content                       
                       unzipped.rdml <- unzip(file.name)
                       tryCatch({
                         if(length(unzipped.rdml) > 1)
                         {
                           rdml.doc <- xmlParse("rdml_data.xml")
                           self$dilutions <- GetDilutionsRoche()
                         }
                         else
                         {
                           rdml.doc <- xmlParse(unzipped.rdml)
                           self$dilutions <- GetDilutions(rdml.doc)
                         }},
                         error = function(e) { print(e) },
                         finally = unlink(unzipped.rdml))
                       ####
                       
                       self$publisher <- GetPublisher(rdml.doc)                       
                       types <- GetSamplesTypes(rdml.doc)
                       if(self$publisher == "Roche Diagnostics") {
                         ntp.sample.id <- names(types)[which(types=="ntp")]
                         reacts.req <- 
                           paste0("/rdml:rdml/rdml:experiment/rdml:run/rdml:react/rdml:sample[@id!='",
                                    ntp.sample.id,
                                    "']/..")
                         qpcr.data.req <- paste0("/rdml:rdml/rdml:experiment/rdml:run/rdml:react/rdml:sample[@id!='",
                                                   ntp.sample.id,
                                                   "']/../rdml:data/rdml:adp/rdml:fluor")
                         melt.data.req <- paste0("/rdml:rdml/rdml:experiment/rdml:run/rdml:react/rdml:sample[@id!='",
                                                   ntp.sample.id,
                                                   "']/../rdml:data/rdml:mdp/rdml:fluor")
                         samples.ids.req <- paste0("/rdml:rdml/rdml:experiment/rdml:run/rdml:react/rdml:sample[@id!='",
                                                     ntp.sample.id,
                                                     "']")
                         reacts.ids.req <- paste0("/rdml:rdml/rdml:experiment/rdml:run/rdml:react/rdml:sample[@id!='",
                                                     ntp.sample.id,
                                                     "']/..")
                         targets.req <- paste0("/rdml:rdml/rdml:experiment/rdml:run/rdml:react/rdml:sample[@id!='",
                                                     ntp.sample.id,
                                                     "']/../rdml:data/rdml:tar")
                       }
                       else {
                         reacts.req <-"/rdml:rdml/rdml:experiment/rdml:run/rdml:react"
                         qpcr.data.req <- "/rdml:rdml/rdml:experiment/rdml:run/rdml:react/rdml:data/rdml:adp/rdml:fluor"
                         melt.data.req <- "/rdml:rdml/rdml:experiment/rdml:run/rdml:react/rdml:data/rdml:mdp/rdml:fluor"
                         samples.ids.req <- "/rdml:rdml/rdml:experiment/rdml:run/rdml:react/rdml:sample"
                         reacts.ids.req <- "/rdml:rdml/rdml:experiment/rdml:run/rdml:react"
                         targets.req <- "/rdml:rdml/rdml:experiment/rdml:run/rdml:react/rdml:data/rdml:tar"                                                  
                       }                       
                       self$plate.dims <- GetPlateDimensions(rdml.doc) 
                       if(self$publisher == "Roche Diagnostics")
                         sdescs <- GetDescriptions(rdml.doc)
                       
                       reacts <- getNodeSet(
                         rdml.doc,
                         reacts.req,
                         namespaces = c(rdml = "http://www.rdml.org"))
                       n.reacts = length(reacts)
                       n.qpcr.cycles <- length(reacts[[1]][["data"]]["adp"])
                       temp.stages <- sapply(reacts[[1]][["data"]]["mdp"], 
                                             function(mdp) {xmlValue(mdp[["tmp"]])})
                       n.temp.stages <- length(temp.stages)
                       data.per.react <- length(reacts[[1]]["data", all = TRUE])
                       if(!is.na(n.qpcr.cycles)) {
                         fqpcr <- as.numeric(xpathSApply( rdml.doc,
                                                          qpcr.data.req,
                                                          xmlValue,
                                                          namespaces = c(rdml = "http://www.rdml.org")))
                         
                         
                         private$qPCR.fdata <- matrix(fqpcr,
                                                      nrow = n.qpcr.cycles,
                                                      dimnames = list(row = 1:n.qpcr.cycles))
                       }
                       if(!is.na(n.temp.stages)) {
                         fmelt <- as.numeric(xpathSApply( rdml.doc,
                                                          melt.data.req,
                                                          xmlValue,
                                                          namespaces = c(rdml = "http://www.rdml.org")))
                         
                         private$melt.fdata <- matrix(fmelt,
                                                      nrow = n.temp.stages,
                                                      dimnames = list(row = temp.stages))
                       }
                       
                                                  
                       samples.ids <- xpathSApply( rdml.doc,
                                                   samples.ids.req,
                                                   xmlGetAttr,
                                                   name = "id",
                                                   namespaces = c(rdml = "http://www.rdml.org"))
                       if(self$publisher == "Roche Diagnostics") tube.names <- sdescs[samples.ids]
                       else tube.names <- samples.ids
                       reacts.ids <- xpathSApply( rdml.doc,
                                                  reacts.ids.req,
                                                  xmlGetAttr,
                                                  name = "id",
                                                  namespaces = c(rdml = "http://www.rdml.org"))
                       targets <- xpathSApply(rdml.doc,
                                    targets.req, 
                                    xmlGetAttr,
                                    name = "id",
                                    namespaces = c(rdml = "http://www.rdml.org"))
                       types <- types[samples.ids]
                       tubes <- sapply(reacts.ids,
                                       function(react.id) { ifelse(                                         
                                         (self$publisher == "StepOne"),
                                         react.id, {
                                           react.id <- as.integer(react.id)
                                           paste0(LETTERS[react.id %/% self$plate.dims["columns"] + 1],
                                                  react.id %% self$plate.dims["columns"])
                                         }                                       
                                       )
                                       }
                       )
                       if(data.per.react != 1) {
                         tube.names <- rep(tube.names, each = data.per.react)
                         reacts.ids <- rep(reacts.ids, each = data.per.react)
                         types <- rep(types, each = data.per.react)
                         tubes <- rep(tubes, each = data.per.react)
                       }
                       private$plate.map <- matrix(nrow = 6,
                                                   ncol = length(targets),
                                                   dimnames = list(row = c("ReactID",                                                    
                                                                           "Tube",
                                                                           "TubeName",
                                                                           "Target",
                                                                           "Type",
                                                                           "FDataName")))
                                                    
                       private$plate.map["TubeName",] <- tube.names
                       private$plate.map["ReactID",] <- reacts.ids
                       private$plate.map["Target",] <- targets
                       private$plate.map["Type",] <- types
                       private$plate.map["Tube",] <- tubes
                       for(i in 1:length(targets)) {
                         private$plate.map["FDataName",i] <- GenSName(name.pattern,
                                                                      self$plate.dims,
                                                                      private$plate.map["ReactID", i],
                                                                      private$plate.map["TubeName", i],
                                                                      private$plate.map["Tube", i],
                                                                      private$plate.map["Target", i],
                                                                      private$plate.map["Type", i],
                                                                      self$publisher)
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
                                                     
                     },
                     qd = function() private$qPCR.fdata,
                     md = function() private$melt.fdata,
                     pm = function() private$plate.map
                   )
                       )
