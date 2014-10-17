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

# Gets concentrations (quantity) of each 
# dilution from XML for Roche
GetDilutionsRoche <- function(uniq.folder)
{
  #   unzipped.rdml <- unzip(file)
  tryCatch({
    rdml.doc <- xmlParse(paste0(uniq.folder,"/calculated_data.xml"))
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

# Generates fluorescence data vector name by specified pattern
GenFDataName <- function (name.pattern,
                          react.id,
                          tube.name,
                          tube,
                          target,
                          type)
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

#' Creates new instance of \code{RDML} class object
#' 
#' This function has been designed to import fluorescence data from instruments 
#' that support RDML v1.1 format export.
#' 
#' @section Warning: Although the format RDML claimed as data exchange format, 
#'   the specific implementation of the format at devices from different 
#'   manufacturers differ significantly. Currently this function is checked 
#'   against RDML data from devices: \emph{Bio-Rad CFX96}, \emph{Roche 
#'   LightCycler 96} and \emph{Applied Biosystems StepOne}.
#' @param rdmlfile RDML file with fluorescence data.
#' @param name.pattern sample name pattern (see 'Details' at \link{RDML}).
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>, Stefan Roediger 
#'   <stefan.roediger@@hs-lausitz.de>, Michal Burdukiewicz 
#'   <michalburdukiewicz@@gmail.com>
#' @docType methods
#' @name new
#' @aliases RDML.new
#' @rdname new-method
#' @include RDML.R
RDML$set("public", "initialize", function(file.name,
                                          name.pattern = "%NAME%__%TUBE%") {
  # Unzips RDML to unique folder to get inner XML content.
  # Unique folder is needed to prevent file ovewriting
  # by parallel function usage.
  uniq.folder <- paste0(".temp/", UUIDgenerate())
  unzipped.rdml <- unzip(file.name, exdir = uniq.folder)
  tryCatch({
    # Roche use more than one file at RDML zip.
    # One of the files store dilutions information.
    if(length(unzipped.rdml) > 1)
    {
      rdml.doc <- xmlParse(paste0(uniq.folder,"/rdml_data.xml"))
      private$.dilutions <- GetDilutionsRoche(uniq.folder)
    }
    else
    {
      rdml.doc <- xmlParse(unzipped.rdml)
      private$.dilutions <- GetDilutions(rdml.doc)
    }},
    error = function(e) { print(e) },
    finally = unlink(uniq.folder, recursive = TRUE)
    )
  ####
  
  # get publisher info
  private$.publisher <- GetPublisher(rdml.doc)
  # get type of each sample
  types <- GetSamplesTypes(rdml.doc)
  
  # Roche file contains empty tubes - "ntp" type.
  # Special request omits them (by corresponding sample name)
  if(private$.publisher == "Roche Diagnostics") {
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
  
  # get plate dimensions
  private$.plate.dims <- GetPlateDimensions(rdml.doc)
  
  # Roche uses generated samples IDs instead of samples names
  # at fluorescense data nodes. Needs to get information how 
  # they corresponds to each other.
  if(private$.publisher == "Roche Diagnostics")
    sdescs <- GetDescriptions(rdml.doc)
  
  # get all reacts containing fluor data
  reacts <- getNodeSet(
    rdml.doc,
    reacts.req,
    namespaces = c(rdml = "http://www.rdml.org"))
  n.reacts = length(reacts)
  # number of cycles
  n.qpcr.cycles <- length(reacts[[1]][["data"]]["adp"])
  # temperature stages for melting
  temp.stages <- sapply(reacts[[1]][["data"]]["mdp"], 
                        function(mdp) {xmlValue(mdp[["tmp"]])})
  n.temp.stages <- length(temp.stages)
  # number of fluorescence data vectors per react.
  # Maybe more than one if publisher stores data for different
  # targets of one sample at one "react".
  data.per.react <- length(reacts[[1]]["data", all = TRUE])
  
  # test if file contains qPCR data
  if(n.qpcr.cycles != 0) {
    # get all fluorescence data as one vector
    fqpcr <- as.numeric(xpathSApply( rdml.doc,
                                     qpcr.data.req,
                                     xmlValue,
                                     namespaces = c(rdml = "http://www.rdml.org")))
    # split this vector by number of cycles
    private$.qPCR.fdata <- matrix(fqpcr,
                                  nrow = n.qpcr.cycles,
                                  dimnames = list(row = 1:n.qpcr.cycles))
  }
  
  # test if file contains melting data
  if(n.temp.stages != 0) {
    # get all fluorescence data as one vector
    fmelt <- as.numeric(xpathSApply( rdml.doc,
                                     melt.data.req,
                                     xmlValue,
                                     namespaces = c(rdml = "http://www.rdml.org")))
    # split this vector by number of temperature stages
    private$.melt.fdata <- matrix(fmelt,
                                  nrow = n.temp.stages,
                                  dimnames = list(row = temp.stages))
  }
  
  # get samples ids (samples names excluding Roche)
  samples.ids <- xpathSApply( rdml.doc,
                              samples.ids.req,
                              xmlGetAttr,
                              name = "id",
                              namespaces = c(rdml = "http://www.rdml.org"))
  # get samples names by samples ids for Roche or use
  # use samples ids as names directly
  if(private$.publisher == "Roche Diagnostics") tube.names <- sdescs[samples.ids]
  else tube.names <- samples.ids
  
  # get react ids (number of tube at plate or 
  # tube name (i.e. "A1") for StepOne )
  reacts.ids <- xpathSApply( rdml.doc,
                             reacts.ids.req,
                             xmlGetAttr,
                             name = "id",
                             namespaces = c(rdml = "http://www.rdml.org"))
  
  # get targets used at each react
  targets <- xpathSApply(rdml.doc,
                         targets.req, 
                         xmlGetAttr,
                         name = "id",
                         namespaces = c(rdml = "http://www.rdml.org"))
  
  # get types used at each react
  types <- types[samples.ids]
  
  # generate tube names (i.e. "A1") or
  #  react ids for StepOne directly
  tubes <- sapply(reacts.ids,
                  function(react.id) { ifelse(                                         
                    (private$.publisher == "StepOne"),
                    react.id, {
                      react.id <- as.integer(react.id)
                      paste0(LETTERS[react.id %/% private$.plate.dims["columns"] + 1],
                             react.id %% private$.plate.dims["columns"])
                    }                                       
                  )
                  }
  )
  
  # if >1 fluorescence data vectors per react,
  # then number of other react parametrs have to be
  # multiplied to correspond length of targets vector
  if(data.per.react != 1) {
    tube.names <- rep(tube.names, each = data.per.react)
    reacts.ids <- rep(reacts.ids, each = data.per.react)
    types <- rep(types, each = data.per.react)
    tubes <- rep(tubes, each = data.per.react)
  }
  
  # create plate map (matrix which stores target, type etc. 
  # of fluor data vectors)
  private$.plate.map <- data.frame(FDataID = 1:length(reacts.ids),
                                   ReactID = reacts.ids,                                                    
                                   Tube = tubes,
                                   TubeName = tube.names,
                                   Target = targets,
                                   Type = types,
                                   FDataName = "")
  self$name.pattern <- name.pattern
}
, overwrite = TRUE)