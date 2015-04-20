FromPositionToId <- function (react.id) {
  row <- which(LETTERS ==
                 gsub("([A-Z])[0-9]+", "\\1", react.id))
  col <- as.integer(gsub("[A-Z]([0-9]+)", "\\1", react.id))
  as.character((row - 1) * 12 + col)
}

GetIds <- function(l) {
  unname(sapply(l, function(el) el$id))
}

# Gets concentrations (quantity) of each 
# dilution from XML for Roche
GetDilutionsRoche <- function(uniq.folder)
{
  cat("\nParsing Roche standards data...")
  if(!file.exists(paste0(uniq.folder,"/calculated_data.xml"))) {
    cat("NO SUCH FILE")
    return(NA)
  }
  rdml.doc <- xmlParse(paste0(uniq.folder,"/calculated_data.xml"))
  concs<-as.numeric(xpathSApply(
    rdml.doc,
    "//ns:absQuantDataSource/ns:standard",   
    namespaces = c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"),
    xmlValue))
  if(length(concs) == 0) {
    concs<-as.numeric(xpathSApply(
      rdml.doc,
      "//ns:relQuantDataSource/ns:standard",   
      namespaces = c(ns = "http://www.roche.ch/LC96RelQuantCalculatedDataModel"),
      xmlValue))
    concs.guids<-xpathSApply(
      rdml.doc,
      "//ns:relQuantDataSource/ns:standard/../ns:graphId",   
      namespaces = c(ns = "http://www.roche.ch/LC96RelQuantCalculatedDataModel"),
      xmlValue)
    names(concs) <- concs.guids
    concs <- sort(concs, decreasing=TRUE)
    positions <- 
      xpathSApply(
        rdml.doc, 
        paste0("//ns:standardPoints/ns:standardPoint/ns:position"), 
        xmlValue,
        namespaces = 
          c(ns = "http://www.roche.ch/LC96RelQuantCalculatedDataModel"))
    positions <- sapply(positions, FromPositionToId)
    dye.names <-xpathSApply(
      rdml.doc, 
      paste0("//ns:standardPoints/ns:standardPoint/ns:dyeName"), 
      xmlValue,
      namespaces = c(ns = "http://www.roche.ch/LC96RelQuantCalculatedDataModel"))
    positions.guids <- xpathSApply(
      rdml.doc, 
      paste0("//ns:standardPoints/ns:standardPoint/ns:graphIds/ns:guid"), 
      xmlValue,
      namespaces = c(ns = "http://www.roche.ch/LC96RelQuantCalculatedDataModel"))
  } else {
    concs.guids<-xpathSApply(
      rdml.doc,
      "//ns:absQuantDataSource/ns:standard/../ns:graphId",   
      namespaces = c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"),
      xmlValue)
    names(concs) <- concs.guids
    concs <- sort(concs, decreasing=TRUE)
    positions <- 
      xpathSApply(
        rdml.doc, 
        paste0("//ns:standardPoints/ns:standardPoint/ns:position"), 
        xmlValue,
        namespaces = 
          c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"))
    positions <- sapply(positions, FromPositionToId)
    dye.names <-xpathSApply(
      rdml.doc, 
      paste0("//ns:standardPoints/ns:standardPoint/ns:dyeName"), 
      xmlValue,
      namespaces = c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"))
    positions.guids <- xpathSApply(
      rdml.doc, 
      paste0("//ns:standardPoints/ns:standardPoint/ns:graphIds/ns:guid"), 
      xmlValue,
      namespaces = c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"))    
  }
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
  if (length(dilutions) == 0) {
    cat("NONE")
    return(NULL)
  }
  names(dilutions) <- dyes
  cat("OK")
  return(dilutions)
}

GetConditionsRoche <- function(uniq.folder)
{
  cat("\nParsing Roche conditions data...")
  if(!file.exists(paste0(uniq.folder,"/app_data.xml"))) {
    cat("NO SUCH FILE")
    return(NA)
  }
  rdml.doc <- xmlParse(paste0(uniq.folder,"/app_data.xml"))
  nodes <- getNodeSet(rdml.doc,
                      "/ns:rocheLC96AppExtension/ns:experiment/ns:run/ns:react/ns:condition/..",
                      namespaces = c(ns = "http://www.roche.ch/LC96AppExtensionSchema"))
  
  reacts <- sapply(nodes,
                   function(node)  xmlAttrs(node, "id"))
  conditions <- sapply(nodes,
                       function(node)  xmlValue(node[["condition"]]))
  if (length(conditions) == 0) {
    cat("NONE")
    return(NULL)
  }
  names(conditions) <- reacts
  cat("OK")
  return(conditions)
}

GetRefGenesRoche <- function(uniq.folder)
{
  cat("\nParsing Roche reference genes data...")
  if(!file.exists(paste0(uniq.folder,"/module_data.xml"))) {
    cat("NO SUCH FILE")
    return(NA)
  }
  rdml.doc <- xmlParse(paste0(uniq.folder,"/module_data.xml"))
  
  ref <- getNodeSet(
    rdml.doc,
    "//ns:geneSettings/ns:relQuantGeneSettings",
    namespaces = c(ns = "http://www.roche.ch/LC96RelQuantGeneralDataModel"))
  
  if (length(ref) == 0) {
    cat("NONE")
    return(NULL)
  }
  
  cat("OK")
  return(ref)
}


#' Creates new instance of \code{RDML} class object
#' 
#' This function has been designed to import data from RDML v1.1 and v1.2 format files
#' 
#' @section Warning: Although the format RDML claimed as data exchange format, 
#'   the specific implementation of the format at devices from real 
#'   manufacturers differ significantly. Currently this function is checked 
#'   against RDML data from devices: \emph{Bio-Rad CFX96}, \emph{Roche 
#'   LightCycler 96} and \emph{Applied Biosystems StepOne}.
#' @param input \code{string} -- path to RDML file
#' @param conditions.sep separator for condition defined at sample name
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>, Stefan Roediger 
#'   <stefan.roediger@@hs-lausitz.de>, Michal Burdukiewicz 
#'   <michalburdukiewicz@@gmail.com>
#' @docType methods
#' @name new
#' @aliases RDML.new
#' @rdname new-method
#' @import XML
#' @include RDML.R
#' @examples
#' \dontrun{
#' ## Import from RDML file
#' PATH <- path.package("RDML")
#' filename <- paste(PATH, "/extdata/", "lc96_bACTXY.rdml", sep ="")
#' lc96 <- RDML$new(filename)
#' 
#' ## Some kind of overview for lc96
#' lc96$AsTable(name.pattern = sample[[react$sample]]$description)
#' lc96$AsDendrogram()
#' }
RDML$set("public", "initialize", function(input,
                                          conditions.sep = NULL) {  
  if(missing(input)) return()
  assert_that(is.string(input))
  # Unzips RDML to unique folder to get inner XML content.
  # Unique folder is needed to prevent file ovewriting
  # by parallel function usage.
  uniq.folder <- tempfile() #paste0(tempdir(), UUIDgenerate())
  cat(sprintf("Unzipping %s...", input))
  unzipped.rdml <- unzip(input, exdir = uniq.folder)
  dilutions.r <- NULL
  ref.genes.r <- NULL
  
  tryCatch({
    # Roche use more than one file at RDML zip.
    # One of the files store dilutions information.
    if(length(unzipped.rdml) > 1)
    {
      cat("\nParsing Roche(?) data...")
      rdml.doc <- xmlParse(paste0(uniq.folder,"/rdml_data.xml"))
      cat("OK")
      dilutions.r <- GetDilutionsRoche(uniq.folder)
      conditions.r <- GetConditionsRoche(uniq.folder)
      ref.genes.r <- GetRefGenesRoche(uniq.folder)
      # private$.dilutions <- dilutions.r
    }
    else
    {
      cat("\nParsing data...")
      rdml.doc <- xmlParse(unzipped.rdml)
      #     private$.dilutions <- GetDilutions(rdml.doc)
    }},
    error = function(e) { print(e) },
    finally = unlink(uniq.folder, recursive = TRUE)
  )
  ####
  
  rdml.root <- xmlRoot(rdml.doc)
  rdml.namespace <- c(rdml = "http://www.rdml.org")
  
  cat("\nGetting dateMade")
  private$.dateMade <- xmlValue(rdml.root[["dateMade"]])
  
  cat("\nGetting dateUpdated")
  private$.dateUpdated <- xmlValue(rdml.root[["dateUpdated"]])
  
  cat("\nGetting id")
  private$.id <- 
    llply(rdml.root["id"],
          function(id) 
            list(publisher = xmlValue(id[["publisher"]]),
                 serialNumber = xmlValue(id[["serialNumber"]]),
                 MD5Hash = xmlValue(id[["MD5Hash"]])
            )
    )
  
  cat("\nGetting experementer")
  private$.experimenter <- {
    experimenter.list <- 
      llply(rdml.root["experimenter"],
            function(el) {            
              list(
                id = xmlAttrs(el, "id"),
                firstName = xmlValue(el[["firstName"]]),
                lastName = xmlValue(el[["lastName"]]),
                email = xmlValue(el[["email"]]),
                labName = xmlValue(el[["labName"]]),
                labAddress = xmlValue(el[["labAddress"]])
              )
            }
      )
    names(experimenter.list) <- GetIds(experimenter.list)
    experimenter.list
  }  
  
  cat("\nGetting documentation")
  private$.documentation <- {
    documentation.list <- 
      llply(rdml.root["documentation"],
            function(el) 
              list(
                id = xmlAttrs(el, "id"),
                text = xmlValue(el[["text"]])
              )          
      )
    names(documentation.list) <- GetIds(documentation.list)
    documentation.list
  }
  
  cat("\nGetting dye")
  private$.dye <- {
    dye.list <- 
      llply(rdml.root["dye"],
            function(el) 
              list(
                id = xmlAttrs(el, "id"),
                description = xmlValue(el[["description"]])
              )
      )
    names(dye.list) <- GetIds(dye.list)
    dye.list
  }
  
  cat("\nGetting sample")
  private$.sample <- {
    sample.list <- 
      llply(rdml.root["sample"],
            function(sample) {
              type <- xmlValue(sample[["type"]])
              ######
              # remove Roche omitted ('ntp') samples
              if(type == "ntp")
                return(NULL)
              #####################
              id <- xmlAttrs(sample, "id")
              list(
                id = id,
                description = xmlValue(sample[["description"]]),
                documentation = llply(sample["documentation"],
                                      function(documentation)
                                        if(!is.null(documentation))
                                          xmlAttrs(documentation, "id")
                ),
                xRef = llply(sample["xRef"],
                             function(xRef) c(
                               name = xmlValue(xRef[["name"]]),
                               id = xmlValue(xRef[["id"]])
                             )),
                annotation = {
                  annotations <- ldply(sample["annotation"],
                                       function(annotation) 
                                         data.frame(
                                           property = xmlValue(annotation[["property"]]),
                                           value = xmlValue(annotation[["value"]]),
                                           stringsAsFactors = FALSE)
                  )
                  if(!is.null(conditions.sep))
                    rbind(annotations,
                          data.frame(
                            property = "condition",
                            value = gsub(sprintf("^.*%s(.*)$",
                                                 conditions.sep),
                                         "\\1", id),
                            stringsAsFactors = FALSE))
                  annotations
                },
                type = type,
                interRunCalibrator = as.logical(xmlValue(sample[["interRunCalibrator"]])),
                quantity = list(value = as.numeric(xmlValue(sample[["quantity"]][["value"]])),
                                unit = xmlValue(sample[["quantity"]][["unit"]])),
                calibratorSample = as.logical(xmlValue(sample[["calibaratorSample"]])),
                cdnaSynthesisMethod = 
                  list(enzyme = xmlValue(sample[["cdnaSynthesisMethod"]][["enzyme"]]),
                       primingMethod = xmlValue(sample[["cdnaSynthesisMethod"]][["primingMethod"]]),
                       dnaseTreatment = as.logical(xmlValue(sample[["cdnaSynthesisMethod"]][["dnaseTreatment"]])),
                       thermalCyclingConditions = { 
                         if(!is.null(sample[["cdnaSynthesisMethod"]][["thermalCyclingConditions"]]))
                           xmlAttrs(sample[["cdnaSynthesisMethod"]][["thermalCyclingConditions"]],
                                    "id")
                       }
                  ),
                templateQuantity = 
                  list(conc = as.numeric(xmlValue(sample[["templateQuantity"]][["conc"]])),
                       nucleotide = xmlValue(sample[["templateQuantity"]][["nucleotide"]]))
              )
            })    
    names(sample.list) <- GetIds(sample.list)
    compact(sample.list)
  }
  
  cat("\nGetting target")
  private$.target <- {
    target.list <- llply(rdml.root["target"],
                         function(target) { 
                           list(
                             id = { 
                               ifelse(length(private$.id) != 0 &&
                                        private$.id[[1]]$publisher == "Roche Diagnostics",
  {
    id <- xmlAttrs(target, "id")
    gsub("@(.+)$", "\\1", 
         regmatches(id,gregexpr("@(.+)$",id))[[1]])
  },
  xmlAttrs(target, "id"))}
  ,
  description = xmlValue(target[["description"]]),
  documentation = llply(target["documentation"],
                        function(documentation)
                          if(!is.null(documentation))
                            xmlAttrs(documentation, "id")
  ),
  xRef = llply(target["xRef"],
               function(xRef) c(
                 name = xmlValue(xRef[["name"]]),
                 id = xmlValue(xRef[["id"]])
               )),
  type = xmlValue(target[["type"]]),
  amplificationEfficiencyMethod = 
    xmlValue(target[["amplificationEfficiencyMethod"]]),
  amplificationEfficiency = 
    as.numeric(xmlValue(target[["amplificationEfficiency"]])),
  amplificationEfficiencySE = 
    as.numeric(xmlValue(target[["amplificationEfficiencySE"]])),
  detectionLimit = 
    as.numeric(xmlValue(target[["detectionLimit"]])),
  dyeId = { 
    id <- xmlAttrs(target[["dyeId"]],"id")
    if(is.null(id)) NA
    else id
  },
  sequences = list(
    forwardPrimer = c(threePrimeTag = 
                        xmlValue(target[["sequences"]][["forwardPrimer"]][["threePrimeTag"]]),
                      fivePrimeTag = 
                        xmlValue(target[["sequences"]][["forwardPrimer"]][["fivePrimeTag"]]),
                      sequence = 
                        xmlValue(target[["sequences"]][["forwardPrimer"]][["sequence"]])),
    reversePrimer = c(threePrimeTag = 
                        xmlValue(target[["sequences"]][["reversePrimer"]][["threePrimeTag"]]),
                      fivePrimeTag = 
                        xmlValue(target[["sequences"]][["reversePrimer"]][["fivePrimeTag"]]),
                      sequence = 
                        xmlValue(target[["sequences"]][["reversePrimer"]][["sequence"]])),
    probe1 = c(threePrimeTag = 
                 xmlValue(target[["sequences"]][["probe1"]][["threePrimeTag"]]),
               fivePrimeTag = 
                 xmlValue(target[["sequences"]][["probe1"]][["fivePrimeTag"]]),
               sequence = 
                 xmlValue(target[["sequences"]][["probe1"]][["sequence"]])),
    probe2 = c(threePrimeTag = 
                 xmlValue(target[["sequences"]][["probe2"]][["threePrimeTag"]]),
               fivePrimeTag = 
                 xmlValue(target[["sequences"]][["probe2"]][["fivePrimeTag"]]),
               sequence = 
                 xmlValue(target[["sequences"]][["probe2"]][["sequence"]])),
    amplicon = c(threePrimeTag = 
                   xmlValue(target[["sequences"]][["amplicon"]][["threePrimeTag"]]),
                 fivePrimeTag = 
                   xmlValue(target[["sequences"]][["amplicon"]][["fivePrimeTag"]]),
                 sequence = 
                   xmlValue(target[["sequences"]][["amplicon"]][["sequence"]]))
  ),
  commercialAssay = list(company = 
                           xmlValue(target[["commercialAssay"]][["company"]]),
                         orderNumber = 
                           xmlValue(target[["commercialAssay"]][["orderNumber"]]))
  
                           )
                         }
    )
    names(target.list) <- GetIds(target.list)
    target.list
  }
  
  cat("\nGetting thermalCyclingConditions")
  private$.thermalCyclingConditions <-{
    tcc.list <- 
      llply(rdml.root["thermalCyclingConditions"],
            function(tcc) {
              list(
                id = xmlAttrs(tcc, "id"),
                description = xmlValue(tcc[["description"]]),
                documentation = llply(tcc["documentation"],
                                      function(documentation)
                                        if(!is.null(documentation))
                                          xmlAttrs(documentation, "id")
                ),
                lidTemperature = 
                  as.numeric(xmlValue(tcc[["lidTemperature"]])),
                experimenter = sapply(tcc["experimenter"],
                                      function(experimenter)
                                        if(!is.null(experimenter))
                                          xmlAttrs(experimenter, "id")
                ),
                step = llply(tcc["step"],
                             function(step) list(
                               nr = as.integer(xmlValue(step[["nr"]])),
                               description = xmlValue(step[["description"]]),
                               temperature = list(
                                 temperature = 
                                   as.numeric(xmlValue(step[["temperature"]][["temperature"]])),
                                 duration = 
                                   as.integer(xmlValue(step[["temperature"]][["duration"]])),
                                 temperatureChange = 
                                   as.numeric(xmlValue(step[["temperature"]][["temperatureChange"]])),
                                 durationChange = 
                                   as.integer(xmlValue(step[["temperature"]][["durationChange"]])),
                                 measure = xmlValue(step[["temperature"]][["measure"]]),
                                 ramp = 
                                   as.numeric(xmlValue(step[["temperature"]][["ramp"]]))
                               ),
                               gradient = list(
                                 highTemperature = 
                                   as.numeric(xmlValue(step[["gradient"]][["highTemperature"]])),
                                 lowTemperature = 
                                   as.numeric(xmlValue(step[["gradient"]][["lowTemperature"]])),
                                 duration = 
                                   as.integer(xmlValue(step[["gradient"]][["duration"]])),
                                 temperatureChange = 
                                   as.numeric(xmlValue(step[["gradient"]][["temperatureChange"]])),
                                 durationChange = 
                                   as.integer(xmlValue(step[["gradient"]][["durationChange"]])),
                                 measure = xmlValue(step[["gradient"]][["measure"]]),
                                 ramp = 
                                   as.numeric(xmlValue(step[["gradient"]][["ramp"]]))
                               ),
                               loop = list(
                                 goto = as.integer(xmlValue(step[["loop"]][["goto"]])),
                                 # should be called "repeat" but this is reserved word
                                 repeatN = as.integer(xmlValue(step[["loop"]][["repeat"]])) 
                               ),
                               pause = list(
                                 temperature = 
                                   as.numeric(xmlValue(step[["pause"]][["temperature"]]))
                               ),
                               lidOpen = xmlValue(step[["lidOpen"]][["lidOpenType"]])                           
                             )
                )
              )
            })
    names(tcc.list) <- GetIds(tcc.list)
    tcc.list
  }
  
  GetData <- function(data, experiment.id, run.id, react.id) {    
    tar.id <- xmlAttrs(data[["tar"]], "id")
    data.req <- paste0("/rdml:rdml/rdml:experiment[@id='",
                       experiment.id,
                       "']/rdml:run[@id='",                                                                         
                       run.id,
                       "']/rdml:react[@id='",
                       #                                                                          react.id[length(react.id)],
                       react.id,
                       "']/rdml:data/rdml:tar[@id='",
                       tar.id,
                       "']/..")                                                      
    list(
      id = ifelse(length(private$.id) != 0 &&
                    private$.id[[1]]$publisher == "Roche Diagnostics",
                  gsub("@(.+)$", "\\1", 
                       regmatches(tar.id,gregexpr("@(.+)$",tar.id))[[1]])
                  ,
                  tar.id), # id==tar
      cq = as.numeric(xmlValue(data[["cq"]])),
      excl = xmlValue(data[["excl"]]),
      adp = {                                                                                    
        cyc <- as.numeric(xpathSApply(rdml.doc,
                                      paste0(data.req,
                                             "/rdml:adp/rdml:cyc"),
                                      xmlValue,
                                      namespaces = c(rdml = "http://www.rdml.org")))
        tmp <- as.numeric(xpathSApply(rdml.doc,
                                      paste0(data.req,
                                             "/rdml:adp/rdml:tmp"),
                                      xmlValue,
                                      namespaces = c(rdml = "http://www.rdml.org")))                                                                        
        fluor <- as.numeric(xpathSApply(rdml.doc,
                                        paste0(data.req,
                                               "/rdml:adp/rdml:fluor"),
                                        xmlValue,
                                        namespaces = c(rdml = "http://www.rdml.org")))
        if(!is.null(fluor)) {
          if(length(tmp) != 0) {
            matrix(c(cyc, tmp, fluor), 
                   byrow = FALSE,
                   ncol = 3,
                   dimnames = list(NULL,
                                   c("cyc", "tmp", "fluor")))
          }
          else {
            matrix(c(cyc, fluor), 
                   byrow = FALSE,
                   ncol = 2,
                   dimnames = list(NULL,
                                   c("cyc", "fluor")))
          }
        } else {
          matrix(ncol = 2,
                 dimnames = list(NULL,
                                 c("cyc", "tmp", "fluor")))
        }
      },
      mdp = {                                                             
        tmp <- as.numeric(xpathSApply(rdml.doc,
                                      paste0(data.req,
                                             "/rdml:mdp/rdml:tmp"),
                                      xmlValue,
                                      namespaces = c(rdml = "http://www.rdml.org")))
        fluor <- as.numeric(xpathSApply(rdml.doc,
                                        paste0(data.req,
                                               "/rdml:mdp/rdml:fluor"),
                                        xmlValue,
                                        namespaces = c(rdml = "http://www.rdml.org")))
        
        if(length(fluor) != 0 && !is.null(fluor))
          matrix(c(tmp, fluor), 
                 byrow = FALSE,
                 ncol = 2,
                 dimnames = list(NULL,
                                 c("tmp", "fluor")))
        else
          matrix(ncol = 2,
                 dimnames = list(NULL,
                                 c("tmp", "fluor")))
      },
      endPt = as.numeric(xmlValue(data[["endPt"]])),
      bgFluor = as.numeric(xmlValue(data[["bgFluor"]])),
      bgFluorSlp = as.numeric(xmlValue(data[["bgFluorSp"]])),
      quantFluor = as.numeric(xmlValue(data[["quantFluor"]]))
    )
  }
  
  GetReact <- function(react, experiment.id, run.id) {
    react.id <- xmlAttrs(react, "id")    
    react.id.corrected <- tryCatch(
      as.character(as.integer(react.id)),
      warning = function(cond) {
        # if react.id is 'B1' not '13'
        # like in StepOne
        FromPositionToId(react.id)
      }    
    )
    #     cat(sprintf("\nreact: %i", react.id))
    sample <- xmlAttrs(react[["sample"]],"id")
    ######
    if(length(private$.id) != 0 && 
       private$.id[[1]]$publisher == "Roche Diagnostics") {
      # remove Roche omitted ('ntp') samples
      if(is.null(private$.sample[[sample]]))
        return(NULL)
      ## Better names for Roche    
      sample <- private$.sample[[xmlAttrs(react[["sample"]],"id")]]$description
    }
    #######    
    list(
      id = react.id.corrected, #sample.id
      # will be calculated at the end of init
      position = NA,
      sample = sample,
      data = {
        data.list <-                                         
          llply(react["data"],
                function(data) GetData(data,
                                       experiment.id,
                                       run.id,
                                       react.id)
          )
        names(data.list) <- GetIds(data.list)
        data.list
      }
    )
  }  
  
  GetRun <- function(run, experiment.id) {
    run.id <-xmlAttrs(run, "id")
    cat(sprintf("\nrun: %s\n", run.id))
    list(
      id = run.id, #xmlAttrs(run, "id"),
      description = xmlValue(run[["description"]]),
      documentation = llply(run["documentation"],
                            function(documentation)
                              if(!is.null(documentation))
                                xmlAttrs(documentation, "id")
      ),
      experimenter = llply(run["experimenter"],
                           function(experimenter)
                             if(!is.null(experimenter))
                               xmlAttrs(experimenter, "id")
      ),
      instrument = xmlValue(run[["instrument"]]),
      dataCollectionSoftware = list(
        name = xmlValue(run[["dataCollectionSoftware"]][["name"]]),
        version = xmlValue(run[["dataCollectionSoftware"]][["version"]])
      ),
      backgroundDeterminationMethod = 
        xmlValue(run[["backgroundDeterminationMethod"]]),
      cqDetectionMethod = 
        xmlValue(run[["cqDetectionMethod"]]),
      thermalCyclingConditions = {
        if(!is.null(run[["thermalCyclingConditions"]]))
          xmlAttrs(run[["thermalCyclingConditions"]], "id")
      },
      pcrFormat = {
        rows <- as.integer(xmlValue(run[["pcrFormat"]][["rows"]]))
        # check for absent of pcrFormat
        # like in StepOne
        if(!is.na(rows)) {
          list(
            rows = rows,
            columns = as.integer(xmlValue(run[["pcrFormat"]][["columns"]])),
            rowLabel = xmlValue(run[["pcrFormat"]][["rowLabel"]]),
            columnLabel = xmlValue(run[["pcrFormat"]][["columnLabel"]])
          )
        } else {
          list(
            rows = 12,
            columns = 8,
            rowLabel = "ABC",
            columnLabel = "123"
          )
        }
      },
      runDate = xmlValue(run[["runDate"]]),
      react = {
        react.list <- 
          llply(run["react"],
                function(react) GetReact(react, 
                                         experiment.id,
                                         run.id),
                .parallel = FALSE,
                .progress = ifelse(interactive(),
                                   "text",
                                   "none")
          )
        names(react.list) <- GetIds(react.list)
        compact(react.list)                                
      }
    )
    
  }                      
  
  GetExperiment <- function(experiment) {
    experiment.id <- xmlAttrs(experiment, "id")
    cat(sprintf("\nGetting experiment: %s", experiment.id))
    list(
      id = experiment.id,
      description = xmlValue(experiment[["description"]]),
      documentation = llply(experiment["documentation"],
                            function(documentation)
                              if(!is.null(documentation))
                                xmlAttrs(documentation, "id")
      ),
      run = {
        run.list <- 
          llply(experiment["run"],
                function(run) GetRun(run, experiment.id)
          )
        
        names(run.list) <- GetIds(run.list)
        run.list
      }
    )
    
  }
  
  
  private$.experiment<- {
    experiment.list <- 
      llply(rdml.root["experiment"],
            function(experiment) GetExperiment(experiment)
      )
    names(experiment.list) <- GetIds(experiment.list)
    experiment.list
  }
  
  private$.recalcPositions()
  
  if(length(private$.id) != 0 && private$.id[[1]]$publisher == "Roche Diagnostics") {    
    for(i in 1:length(private$.sample)) {
      private$.sample[[i]]$id <- private$.sample[[i]]$description
    }
    names(private$.sample) <- GetIds(private$.sample)
    
    cat("Adding Roche ref genes\n")
    if(!is.null(ref.genes.r) && length(ref.genes.r) != 0) {
      for(ref.gene in ref.genes.r) {
        geneName <- xmlValue(ref.gene[["geneName"]])
        geneI <- grep(
          sprintf("^%s$", geneName),
          names(private$.target))
        private$.target[[geneI]]$type <-
          ifelse(as.logical(xmlValue(ref.gene[["isReference"]])),
                 "ref",
                 "toi")
      }
    }
    
    tbl <- self$AsTable()
    cat("Adding Roche quantities\n")
    for(target in dilutions.r %>% names) {
      for(r.id in dilutions.r[[target]] %>% names) {
        sample.name <- filter(tbl, react.id == r.id)$sample[1]
        private$.sample[[sample.name]]$quantity <- list(
          value = unname(dilutions.r[[1]][r.id]),
          unit = "other"
        )
        private$.sample[[sample.name]]$annotation <- 
          rbind(private$.sample[[sample.name]]$annotation,
                data.frame(
                  property = sprintf("Roche_quantity_at_%s_%s",
                                     target,
                                     r.id),
                  value = dilutions.r[[target]][r.id],
                  stringsAsFactors = FALSE))
      }
    }
    
    cat("Adding Roche conditions\n")
    for(r.id in conditions.r %>% names) {
      sample.name <- filter(tbl, react.id == r.id)$sample[1]
      private$.sample[[sample.name]]$annotation <- 
        rbind(private$.sample[[sample.name]]$annotation,
              data.frame(
                property = sprintf("Roche_condition_at_%s",r.id),
                value = conditions.r[r.id],
                stringsAsFactors = FALSE))
    }
    
  }
}, 
overwrite = TRUE)