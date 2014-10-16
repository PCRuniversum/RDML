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

InitRDML <- function(self, private, file.name) {
  # Unzips RDML to unique folder to get inner XML content.
  # Unique folder is needed to prevent file ovewriting
  # by parallel function usage.
  uniq.folder <- UUIDgenerate()  
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
    finally = unlink(uniq.folder, recursive = TRUE))
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
  return(list(self = self,
              private = private))
}

#' R6 class \code{RDML} -- contains methods to read and overview fluorescence 
#' data from RDML v1.1 format files
#' 
#' Main purpose of this \code{class} is to import fluorescence data from RDML 
#' v1.1 format files (Lefever et al. 2009) and transform it to the appropriate 
#' format of the \code{qpcR} (Ritz et al. 2008, Spiess et al. 2008) and 
#' \code{chipPCR} packages (see \link{new.RDML} for import details). Real-time 
#' PCR Data Markup Language (RDML) is the recommended file format element in the
#' Minimum Information for Publication of Quantitative Real-Time PCR Experiments
#' (MIQE) guidelines (Bustin et al. 2009). After importing Fluorescence data 
#' from RDML file can be viewed as \code{data.frame} which contains 
#' \code{vectors} of fluroscence values filtered by experiment method (qPCR or 
#' melting), targets (genes or dyes names), types (i.e. 'negative', 'unknown'), 
#' tube positions on plate, samples names or fluorescence data \code{vector} 
#' names (see \link{GetFData}). Also imported data can be overviewed by 
#' \code{summary} and \code{plot} \code{S3} functions (see \link{summary.RDML} 
#' and \link{plot.RDML}).
#' 
#' Names of the fluorescence data \code{vectors} can be generated by rules 
#' described in \code{name.pattern}. For example, if \code{name.pattern} = 
#' \code{"\%NAME\%__\%TUBE\%"}, sample name (inputted in the PCR software) is 
#' "Sample_1", and position on the plate is "A4", then generated fluorescence 
#' data name will be: "Sample_1__A4".
#' 
#' Possible keys in \code{name.pattern} \describe{ \item{\code{\%NAME\%}}{name 
#' of the sample inputted in the qPCR software (ex.: "Sample 1")} 
#' \item{\code{\%ID\%}}{tube ID (ex.: "23")} \item{\code{\%TUBE\%}}{tube name 
#' (ex.: "B11")} \item{\code{\%TARGET\%}}{PCR target (ex.: "GAPDH")} 
#' \item{\code{\%TYPE\%}}{type of the sample (ex.: "unkn")}  }
#' 
#' @name RDML.class
#' @aliases RDML.class RDML
#' @docType class
#' @section Fields: \describe{ \item{publisher}{name of PCR device manufacturer 
#'   } \item{dilutions}{\code{list} of dillutions with their positions at plate 
#'   splitted by targets } \item{plate.dims}{plate dimensions -- number of rows 
#'   and columns at plate} \item{used.methods}{methods used at experiment --
#'   'qPCR' and/or 'melt'} \item{targets}{targets (genes or dyes names) used by 
#'   experiment} \item{types}{ \code{list} of samples types splitted by targets}
#'   \item{name.pattern}{ name pattern to generate fluorescence data vectors 
#'   names (see 'Details') } } All the fields are read only except 
#'   \code{name.pattern}!!!.
#' @section Methods: \describe{\item{new}{creates new instance of \code{RDML} 
#'   class object (see \link{new.RDML})} \item{GetFData}{gets fluorescence data 
#'   vectors (see \link{GetFData})} }
#'   
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>, Stefan Roediger 
#'   <stefan.roediger@@hs-lausitz.de>, Michal Burdukiewicz 
#'   <michalburdukiewicz@@gmail.com>
#' @references RDML format http://www.rdml.org/ \code{R6} package 
#'   http://cran.r-project.org/web/packages/R6/index.html
#'   
#'   \code{qpcR} package http://cran.r-project.org/web/packages/qpcR/index.html
#'   
#'   \code{chipPCR} package: 
#'   http://cran.r-project.org/web/packages/chipPCR/index.html
#'   
#'   Ritz, C., Spiess, A.-N., 2008. qpcR: an R package for sigmoidal model 
#'   selection in quantitative real-time polymerase chain reaction analysis. 
#'   \emph{Bioinformatics} 24, 1549--1551. doi:10.1093/bioinformatics/btn227
#'   
#'   Spiess, A.-N., Feig, C., Ritz, C., 2008. Highly accurate sigmoidal fitting 
#'   of real-time PCR data by introducing a parameter for asymmetry. \emph{BMC 
#'   Bioinformatics} 9, 221. doi:10.1186/1471-2105-9-221
#'   
#'   Bustin, S.A., Benes, V., Garson, J.A., Hellemans, J., Huggett, J., Kubista,
#'   M., Mueller, R., Nolan, T., Pfaffl, M.W., Shipley, G.L., Vandesompele, J., 
#'   Wittwer, C.T., 2009. The MIQE guidelines: minimum information for 
#'   publication of quantitative real-time PCR experiments. \emph{Clin. Chem.} 
#'   55, 611--622.  doi:10.1373/clinchem.2008.112797
#'   
#'   Lefever, S., Hellemans, J., Pattyn, F., Przybylski, D.R., Taylor, C., 
#'   Geurts, R., Untergasser, A., Vandesompele, J., RDML consortium, 2009. RDML:
#'   structured language and reporting guidelines for real-time quantitative PCR
#'   data.  \emph{Nucleic Acids Res.} 37, 2065--2069. doi:10.1093/nar/gkp056
#' @keywords Bio--Rad CFX96 file IO LightCycler qPCR RDML StepOne
#' @export
#' @importFrom chipPCR MFIaggr
#' @importMethodsFrom chipPCR summary
#' @importFrom MBmca diffQ2
#' @importFrom shiny runApp
#' @importFrom XML getNodeSet xmlGetAttr xmlParse xmlValue xpathSApply
#' @importFrom digest digest
#' @importFrom stringr str_extract
#' @importFrom dpcR plot_panel create_dpcr extract_dpcr
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#' @examples
#' 
#' ## EXAMPLE 1:
#' ## internal dataset lc96_bACTXY.rdml (in 'data' directory)
#' ## generated by Roche LightCycler 96. Contains qPCR data
#' ## with four targets and two types.
#' ## Import with default settings.
#' PATH <- path.package("RDML")
#' filename <- paste(PATH, "/extdata/", "lc96_bACTXY.rdml", sep ="")
#' lc96 <- RDML$new(filename)
#' 
#' ## Show targets names
#' targets <- lc96$targets
#' targets
#' ## Make names easier to type
#' names(targets) <- c("famatbact", "hexatx", "texasredaty", "cy5atipc")
#' ## Show types of the samples for target 'famatbact'
#' lc96$types[[targets["famatbact"]]]
#' 
#' ## Show dilutions for dye - FAM
#' lc96$dilutions$FAM
#' \dontrun{
#' COPIES <- unique(lc96$dilutions$FAM)
#' ## Define calibration curves (type of the samples - 'std').
#' ## No replicates.
#' library(qpcR)
#' CAL <- modlist(lc96$GetFData(filter=list(targets=targets["famatbact"],
#'                                          types="std", method="qPCR")),
#'                fluo = c(2, 4, 6, 8, 10))
#' ## Define samples to predict (first two samples with the type - 'unkn').
#' PRED <- modlist(lc96$GetFData(filter=list(targets=targets["famatbact"],
#'                                          types="std", method="qPCR")),
#'                 fluo = 2:3)
#' ## Conduct quantification.
#' calib(refcurve = CAL, predcurve = PRED, thresh = "cpD2",
#'       dil = COPIES)
#' }
#' \dontrun{
#' ## EXAMPLE 2:
#' ## internal dataset lc96_bACTXY.rdml (in 'data' directory)
#' ## generated by Roche LightCycler 96. Contains qPCR data
#' ## with four targets and two types.
#' ## Import with default settings.
#' library(chipPCR)                        
#' PATH <- path.package("RDML")
#' filename <- paste(PATH, "/extdata/", "lc96_bACTXY.rdml", sep ="")
#' lc96 <- RDML$new(filename)
#' 
#' ## Compactly display the structure of the lc96 object
#' #!! str(lc96)
#' lc96$types
#' ## Make names easier to type
#' names(targets) <- c("famatbact", "hexatx", "texasredaty", "cy5atipc")
#' ## Fetch cycle dependent fluorescence for HEX chanel
#' tmp <- lc96$GetFData(filter = list(method = "qPCR",
#'                                    targets = targets["hexatx"],
#'                                    types = "std"))
#' ## Fetch vector of dillutions for HEX chanel
#' dilution <- as.vector(lc96$dilutions$Hex)
#' 
#' ## Use plotCurves function from the chipPCR package to 
#' ## get an overview of the amplification curves
#' plotCurves(tmp[,1], tmp[,-1])
#' par(mfrow = c(1,1))
#' ## Use inder function from the chipPCR package to 
#' ## calculate the Cq (second derivative maximum, SDM)
#' SDMout <- sapply(2L:ncol(tmp), function(i) {
#'   SDM <- summary(inder(tmp[, 1], tmp[, i]), print = FALSE)[2]
#' })
#' 
#' ## Use the effcalc function from the chipPCR package and 
#' ## plot the results for the calculation of the amplification
#' ## efficiency analysis.
#' plot(effcalc(dilution, SDMout), CI = TRUE)
#' }
#' \dontrun{
#' ## EXAMPLE 3:
#' ## internal dataset BioRad_qPCR_melt.rdml (in 'data' directory)
#' ## generated by Bio-Rad CFX96. Contains qPCR and melting data.
#' ## Import with custom name pattern.
#' PATH <- path.package("RDML")
#' filename <- paste(PATH, "/extdata/", "BioRad_qPCR_melt.rdml", sep ="")
#' cfx96 <- RDML$new(filename,
#'               name.pattern = "%TUBE%_%NAME%_%TYPE%_%TARGET%")
#' ## Use plotCurves function from the chipPCR package to 
#' ## get an overview of the amplification curves
#' library(chipPCR)
#' ## Extract all qPCR data without splitting by targets and types
#' cfx96.qPCR <- cfx96$GetFData()
#' plotCurves(cfx96.qPCR[,1], cfx96.qPCR[,-1], type = "l")
#' 
#' ## Extract all melting data without splitting by targets and types
#' cfx96.melt <- cfx96$GetFData(filter = list(method = "melt"))
#' ## Show some generated names for samples.
#' names(cfx96.melt[2L:5])
#' ## Select columns that contain
#' ## samples with dye 'EvaGreen' and have type 'pos'
#' ## using filtering by names.
#' cols <- cfx96$GetFData(filter = list(method = "melt",
#'                                      names = "pos_EvaGreen$"))
#' ## Conduct melting curve analysis.
#' library(qpcR)
#' invisible(meltcurve(cols, fluos = 2:length(cols),
#'           temps = rep(1, length(cols) - 1)))
#' }
RDML <- R6Class("RDML",
                public = list(
                  initialize = function(file.name,
                                        name.pattern = "%NAME%__%TUBE%") {
                    init <- InitRDML(self, private, file.name)
                    self <- init[["self"]]
                    private <- init[["private"]]
                    self$name.pattern <- name.pattern
                  },
                  GetFData = function(filter = list(method = "qPCR")) {
                    params.names <- names(filter)
                    if(!("method" %in% params.names))
                      filter$method <- "qPCR"
                    if("react.ids" %in% params.names)
                      filtered.indices <- which(
                        private$.plate.map$ReactID %in% as.character(
                          filter$react.ids))
                    else if("tubes" %in% params.names)
                      filtered.indices <- which(
                        private$.plate.map$Tube %in% filter$tubes)
                    else { 
                      filtered.map <- private$.plate.map
                      if("targets" %in% params.names)
                        filtered.map <- filtered.map[which(
                          filtered.map$Target %in% filter$targets),]
                      if("types" %in% params.names)
                        filtered.map <-filtered.map[
                          which(filtered.map$Type %in% filter$types),]
                      if("names" %in% params.names)
                        filtered.map <-filtered.map[
                          grep(filter$names, 
                               filtered.map$FDataName),]
                    }
                    filtered.fdata.ids <- filtered.map$FDataID
                    if(!is.null(filter$method) && filter$method == "melt") {
                      filtered <- as.data.frame(private$.melt.fdata[,filtered.fdata.ids])
                      filtered <- cbind(as.numeric(rownames(private$.melt.fdata)),
                                        filtered)
                      names(filtered) <- c("Temp.", 
                                           filtered.map$FDataName)
                    }
                    else {
                      filtered <- as.data.frame(
                        private$.qPCR.fdata[,filtered.fdata.ids],
                        row.names = rownames(private$.qPCR.fdata))
                      filtered <- cbind(as.numeric(rownames(private$.qPCR.fdata)),
                                        filtered)
                      names(filtered) <- c("Cycles", 
                                           filtered.map$FDataName)
                    }
                    return(filtered)
                  },
                  Summarize = function(print = TRUE, ...) {
                    Create.RDML.summary(self, private, print, ...)
                  },
                  Plot = function(print.legend = TRUE,
                                  separate.by = list(left = c("name", "type", "targets"),
                                                     right = c("name", "type", "targets")),
                                  col = list(left = NA,
                                             right = NA),
                                  empty.col = "white",
                                  ...) {
                    Create.RDML.plot(self, private,
                                     print.legend,
                                     separate.by,
                                     col,
                                     empty.col,
                                     ...)
                  }
                ),
                private = list(
                  .publisher = NA,
                  .dilutions = NA,
                  .plate.dims = NA, 
                  .qPCR.fdata = NA,                  
                  .melt.fdata = NA,
                  .plate.map = NA,
                  .name.pattern = NA
                ),
                active = list(
                  md = function() private$.melt.fdata,
                  publisher = function(value) {
                    if(missing(value))
                      return(private$.publisher)
                    stop("RDML package is not an editor!!! Use RDML-Ninja!!!")
                  },
                  dilutions = function(dilutions) {
                    if(missing(dilutions))
                      return(private$.dilutions)
                    stop("RDML package is not an editor!!! Use RDML-Ninja!!!")
                    #                     private$.dilutions <- dilutions
                  },
                  plate.dims = function(value) {
                    if(missing(value))
                      return(private$.plate.dims)
                    stop("RDML package is not an editor!!! Use RDML-Ninja!!!")
                  },
                  used.methods = function(value) {
                    if(missing(value)) {
                      methods <- c()
                      if(!(is.na(private$.qPCR.fdata))) 
                        methods <- c(methods, "qPCR")                      
                      if(!(is.na(private$.melt.fdata)))
                        methods <- c(methods, "melt")
                      return(methods)
                    }
                    stop("RDML package is not an editor!!! Use RDML-Ninja!!!")
                  },
                  targets = function(targets.names) {
                    if(missing(targets.names))
                      return(as.vector(unique(private$.plate.map$Target)))
                    stop("RDML package is not an editor!!! Use RDML-Ninja!!!")
                    #                     levels(private$.plate.map$Target) <- targets.names
                  },
                  types = function(types.names) { 
                    if(missing(types.names)) {
                      targets <- as.vector(unique(private$.plate.map$Target))                    
                      types <- lapply(targets,
                                      function(target) {
                                        as.vector(unique(private$.plate.map$Type[
                                          which(private$.plate.map$Target == target)
                                          ]))
                                      })
                      names(types) <- targets
                      return(types)
                    }
                    stop("RDML package is not an editor!!! Use RDML-Ninja!!!")
                    #                     private$.plate.map$Type <- as.character(private$.plate.map$Type)
                    #                     for(target in names(types.names)) {
                    #                       types <- as.vector(unique(private$.plate.map$Type[
                    #                         which(private$.plate.map$Target == target)
                    #                         ]))
                    #                       for(type.i in 1:length(types)) {
                    #                         private$.plate.map$Type[which(
                    #                           private$.plate.map$Target == target & 
                    #                             private$.plate.map$Type == types[type.i])] <- 
                    #                           types.names[[target]][type.i]                        
                    #                       }                      
                    #                     }
                    #                     private$.plate.map$Type <- as.factor(private$.plate.map$Type)
                  },                  
                  name.pattern = function(name.pattern) {
                    if(missing(name.pattern))
                      return(private$.name.pattern)
                    private$.name.pattern <- name.pattern
                    private$.plate.map$FDataName <- 
                      sapply(1:nrow(private$.plate.map),
                             function(i) {
                               GenFDataName(private$.name.pattern,
                                            private$.plate.map$ReactID[i],
                                            private$.plate.map$TubeName[i],
                                            private$.plate.map$Tube[i],
                                            private$.plate.map$Target[i],
                                            private$.plate.map$Type[i]
                               )
                             }
                      )
                  }                  
                )
)


Create.RDML.summary <- function(self, private, print, ...) {
  #### Info about dilutions
  if(!is.null(private$.dilutions)) {
    dilTable <- do.call(rbind, private$.dilutions)
    rownames(dilTable) <- names(private$.dilutions)
    if(print) {
      cat("Dilutions:\n")
      print(dilTable)
      cat("\n")
    }
  }
  
  #### Pretty structure of run
  
  runStr <- c()
  for(target in self$targets) {  
    for(stype in self$types[[target]]) {
      runStr <- rbind(runStr, c(Targets = target, Types = stype, 
                                No.Samples = 
                                  (length(self$GetFData(filter = 
                                                          list(targets = target,
                                                               types = stype)))-1)))      
    }
  }
  runStr <- as.data.frame(runStr)
  if(print) {
    cat("\nStructure of run:\n")
    print(runStr) 
    cat("\n")
  }
  
  
  #### Summary for melting data
  if("melt" %in% self$used.methods) {
     
      data <- self$GetFData(filter = list(
        method = "melt"))
      meltList <- suppressMessages(apply(data[, -1], 2, function(i)
        diffQ2(cbind(data[, 1], i))))
    
    meltTable <- matrix( 
      sapply(meltList, function(experiment)
        c(experiment[["Tm"]], experiment[["fluoTm"]],
          experiment[["xTm1.2.D2"]], experiment[["yTm1.2.D2"]])), 
      ncol = 6, byrow = TRUE)
    meltTable <- cbind(private$.plate.map$FDataName, data.frame(meltTable))
    meltTable <- cbind(private$.plate.map$Type, meltTable)
    meltTable <- cbind(private$.plate.map$Target, meltTable)
    
    colnames(meltTable) <- c("Target", "Type", "Experiment", "Tm1D1", "FluoTm1", "Tm1D2", 
                             "D2", "FluoTm1D2", "FluoTm2D2")
        
    if(print) {
      cat("\nTable of melting temperatures:\n")
      print(meltTable) 
      cat("\n")
    }
  }
  #  Summary for qPCR data
  if("qPCR" %in% self$used.methods) {
    
    #iterate MFI aggr over all experiments and all types of experiments
    expTable <- data.frame(do.call(rbind, lapply(self$targets, function(target) {
      tab <- do.call(rbind, 
                     lapply(self$types[[target]], 
                            function(type) {
        c(Target = target, Type = type, summary(
          MFIaggr(self$GetFData(filter = list(method = "qPCR",
                                                    targets = target,
                                                    types = type))), print = FALSE))
        
                            }
        ))
      tab
    })), row.names = NULL)
    
    if(print) {
      #first three moments
      firstThree <- expTable[, c("Target", "Type", "mean", "sd", "skewness")]
      colnames(firstThree) <- c("Target", "Type", "Mean", 
                                "Standard deviation", "Skewness")
      cat("\nSummarized experiments - first three moments:\n")
      print(firstThree)
      
      #resistant statistics
      resistStats <- expTable[, c("Target", "Type", "median", "mad", 
                                  "IQR", "medcouple")]
      colnames(resistStats) <- c("Target", "Type", "Median", 
                                 "Median Absolute Deviation", 
                                 "Interquartile Range", "Medcouple")
      cat("\nSummarized experiments - resistant statistics:\n")
      print(resistStats)
      
      #linear
      linStats <- expTable[, c("Target", "Type", "intercept", "slope", 
                               "r.squared", "heter.p")]
      colnames(linStats) <- c("Target", "Type", "Intercept", "Slope", 
                              "R squared", "Breusch-Pagan Test p-value")
      cat("\nSummarized experiments - linear model statistics:\n")
      print(linStats)
      
      #rest
      restStats <- expTable[, c("Target", "Type", "SNR", "VRM", "NAs")]
      colnames(restStats) <- c("Target", "Type", "SNR", "VRM", 
                               "Number of NAs")
      cat("\nSummarized experiments - other statistics:\n")
      print(restStats)
      
      cat("\n")
      
    }
  }
  res <- list()
  
  if(exists("dilTable"))
    res <- c(res, dilTable = list(dilTable))
  
  res <- c(res, runStr = list(runStr))
  
  if(exists("meltTable"))
    res <- c(res, meltTable = list(meltTable))
  
  if(exists("expTable"))
    res <- c(res, expTable = list(expTable))
  
  invisible(res)
}

Create.RDML.plot <- function(self, private,
                             print.legend,
                             separate.by,
                             col,
                             empty.col,
                             ...) {
  for(half in c("left", "right")) {
    for(opt in separate.by[[half]]) {      
      if(!opt %in%  c("name", "type", "targets", "", NA))
        stop(paste0(c("'", opt, "' is not correct option for separate.by!!!")))
    }
  }
  cnum = private$.plate.dims["columns"]
  rnum = private$.plate.dims["rows"]
  tubes <- unique(private$.plate.map$Tube)
  ## create matrix for left and right values of tubes
  matr <- matrix(ncol = 2,
                 nrow = cnum * rnum)
  colnames(matr) <- c("left", "right")
  
  ## create matrices for types of tubes (defined by separate.by
  ## parametres) at plate 
  tube.types.template <- matrix(nrow = 4,
                                dimnames = list(c("Hash",
                                                  "Name",
                                                  "Type",
                                                  "Targets")))
  tube.types <- list(left = tube.types.template,
                     right = tube.types.template)
  
  ## create matrix for plot legend
  legend <- matrix(ncol = 7, nrow = length(tubes),
                   dimnames = list(c(), c("Left ID",
                                          "L. Color",
                                          "Right ID",
                                          "R. Color",
                                          "Tube Name",
                                          "Type",
                                          "Targets")))
    
  for(tube in tubes) {
    col.index <- as.integer(str_extract(tube,
                                        "[[:digit:]]+"))
    row.index <- which(LETTERS == 
                         str_extract(tube, 
                                     "[[:upper:]]"))
    
    plate.map.by.tube <- private$.plate.map[private$.plate.map$Tube == tube,]    
    index <- which(tubes == tube)    
    legend[index,"Tube Name"] <- as.character(plate.map.by.tube$TubeName[1])
    legend[index,"Type"] <- as.character(plate.map.by.tube$Type[1])
    legend[index,"Targets"] <- paste(plate.map.by.tube$Target,
                                     collapse = "; ")
    
    for(half in c("left", "right")) {
      sname<- ifelse("name" %in% separate.by[[half]],
                     plate.map.by.tube$TubeName[1],
                     NA)
      ttype <- plate.map.by.tube$Type[1]
      ## remove variable name
      names(ttype) <- ""
      ttype <- ifelse("type" %in% separate.by[[half]],
                      ttype,
                      NA)
      ## collapse all targets to one string
      targets <- ifelse("targets" %in% separate.by[[half]],
                        paste(plate.map.by.tube$Target,
                              collapse = "; "),
                        NA)    
      
      ttube <- c(sname, ttype, targets)
      ## generate hash of tube
      tube.hash <- digest(ttube, algo = "crc32")
      
      ## test if tube with given hash already exists and
      ## If not exists - add tube to tube.types matrix
      tube.type.index <- which(tube.types[[half]]["Hash",] == tube.hash)
      if(length(tube.type.index) == 0) {        
        if(tube == tubes[1]) tube.types[[half]][,1] <- c(tube.hash, ttube)
        else tube.types[[half]] <- cbind(tube.types[[half]], 
                                         c(tube.hash, ttube))
        tube.type.index <- length(tube.types[[half]]["Hash",])
      }
      
      ## add type of tube (index of tube hash at tube.types) 
      ## matr
      matr[row.index + (col.index - 1)  * rnum, half] <- tube.type.index        
      
      ## add type off tube (index of tube hash at tube.types) 
      ## to legend
      if(half == "left")
        legend[index,"Left ID"] <- tube.type.index
      else
        legend[index,"Right ID"] <- tube.type.index
      
    }
    
  }
  ## generate random colors for cells
  for(half in c("left", "right")) {
    if(is.na(col[[half]]) || col[[half]] == "") col[[half]] <- NA
    if(is.na(col[[half]]) || 
         (length(col[[half]]) < length(tube.types$left["Hash",]) )) {
      cl <- colors()
      if(!is.na(col[[half]])) {
        col[[half]] <- c(col[[half]], cl[runif(length(tube.types[[half]]["Hash",]) - length(col[[half]]),
                                               1, length(cl))])
      }
      else {
        col[[half]] <- cl[runif(length(tube.types[[half]]["Hash",]),
                                1, length(cl))]
      }
    }
    ## add used colors to legend
    if(half == "left") 
      legend[,"L. Color"] <- sapply(legend[,"Left ID"],
                                    function(col.id) {
                                      col[[half]][as.integer(col.id)]})
    else legend[,"R. Color"] <- sapply(legend[,"Right ID"],
                                       function(col.id) {
                                         col[[half]][as.integer(col.id)]})
    col[[half]] <- c(empty.col , col[[half]])
  }
  
  matr[is.na(matr)] <- 0
  matr.dpcr <- create_dpcr(matr, n = 1L, type = "nm")
  
  ########## plotting
  ## plot left halfs of tubes
  plot_panel(extract_dpcr(matr.dpcr, "left"), 
             nx_a = cnum,
             ny_a = rnum,
             col = col[["left"]],
             legend = FALSE, 
             half = "left",
             use_breaks = FALSE,
             ...)
  par(new = TRUE)
  ## plot right halfs of tubes
  plot_panel(extract_dpcr(matr.dpcr, "right"),
             nx_a = cnum,
             ny_a = rnum,
             col = col[["right"]],
             legend = FALSE,
             half = "right",
             use_breaks = FALSE,
             ...)
  ## add axes
  box()
  # cols
  axis(side = 3, at = 1:cnum, labels = 1:cnum, ...)
  # rows
  axis(side = 2, at = 1:rnum, labels = rev(LETTERS[1:rnum]),
       las = 1, # the style of axis labels: always horizontal.
       ...)
  ########## end plotting
  
  legend <- as.data.frame(legend)
  
  if(print.legend) {
    cat("\nPlot legend:\n")
    print(legend)
    cat("\n")
  }  
  
  invisible(legend)
}