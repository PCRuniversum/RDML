FromPositionToId <- function (react.id) {
  row <- which(LETTERS ==
                 gsub("([A-Z])[0-9]+", "\\1", react.id))
  col <- as.integer(gsub("[A-Z]([0-9]+)", "\\1", react.id))
  (row - 1) * 12 + col
}

GetIds <- function(l) {
  unname(sapply(l, function(el) el$id))
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

#' Creates new instance of \code{RDML} class object
#' 
#' This function has been designed to import fluorescence data from instruments 
#' that support RDML v1.1 format export or create new \code{RDML} class object 
#' from fluorescence data represented as table.
#' 
#' Parameter \code{input} of this method can be \code{character} string for 
#' importing from RDML v1.1 file or \code{data.frame}/\code{list} for creating 
#' \code{RDML} object from table. Choosing variant of \code{input} parameter 
#' depends on samples descpription information that you have and want to use. If
#' you have only fluoresence data and samples names then you can use 
#' \code{data.frame}. First column of such \code{data.frame} have to be named 
#' 'Cycles' for qPCR data or 't' for melting data and to contain cycles numbers 
#' or temperature values respectively. All other columns have to contain 
#' fluoresence data (names of this columns will be names of samples). If you 
#' have information about samples position on plate, type, target -- you have 
#' provide \code{input} as \code{list}. Such \code{list} have to contain tables 
#' with fluorescence data called 'qpcr' for qPCR data and/or table called 'melt'
#' for melting data. First columns of that tables have to contain cycles numbers
#' or temperature. Also \code{list} have to contain \code{data.frame} called 
#' 'map' with addition information about samples. Structure of this 
#' \code{data.frame} is \describe{ \item{name}{sample name} \item{tube.id}{id of
#' tube at plate (i.e. 1, 2, 3)} \item{target}{target of this fluorescence data}
#' \item{dye}{dye used for this target} \item{type}{type of the sample}} Another
#' element of \code{list} is 'dims' -- dimensions of plate (see 'Example 3').
#' 
#' @section Warning: Although the format RDML claimed as data exchange format, 
#'   the specific implementation of the format at devices from different 
#'   manufacturers differ significantly. Currently this function is checked 
#'   against RDML data from devices: \emph{Bio-Rad CFX96}, \emph{Roche 
#'   LightCycler 96} and \emph{Applied Biosystems StepOne}.
#' @param input \code{character} -- path to RDML file
#' @param input \code{data.frame} -- \code{data.frame} with colnames as samples 
#'   names and first column -- cycle number or temperature (see 'Details')
#' @param input \code{list} -- \list{list} with table called 'qPCR' for qPCR 
#'   data and/or table called 'melt' for melting data and \code{data.frame} 
#'   'map' with description of samples (see 'Details')
#' @param name.pattern sample name pattern (see 'Details' at \link{RDML}).
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>, Stefan Roediger 
#'   <stefan.roediger@@hs-lausitz.de>, Michal Burdukiewicz 
#'   <michalburdukiewicz@@gmail.com>
#' @docType methods
#' @name new
#' @aliases RDML.new
#' @rdname new-method
#' @include RDML.R
#' @examples
#' ## EXAMPLE 1
#' ## Import from RDML file
#' PATH <- path.package("RDML")
#' filename <- paste(PATH, "/extdata/", "lc96_bACTXY.rdml", sep ="")
#' lc96 <- RDML$new(filename)
#' 
#' ## EXAMPLE 2
#' ## Import from data.frame
#' library(qpcR)
#' rdml <- RDML$new(reps)
#' 
#' ## EXAMPLE 3
#' ## Import from list
#' qpcr <- C17[-1]
#' smap <- data.frame(name = c("sample_a", "sample_a", "sample_b"),
#'                 tube.id = c("1", "1", "7"),
#'                 target = c("GAPDH", "bACT", "GAPDH"),
#'                 dye = c("FAM", "VIC", "FAM"),
#'                 type = c("unkn", "unkn", "unkn"))
#' rdml <- RDML$new(list(map = smap,                  
#'                     qPCR=qpcr,
#'                     dims=c(rows=5,columns=6)))
RDML$set("public", "initialize", function(input) {  
  assert_that(is.string(input))
  # Unzips RDML to unique folder to get inner XML content.
  # Unique folder is needed to prevent file ovewriting
  # by parallel function usage.
  uniq.folder <- paste0(".temp/", UUIDgenerate())
  unzipped.rdml <- unzip(input, exdir = uniq.folder)
  tryCatch({
    # Roche use more than one file at RDML zip.
    # One of the files store dilutions information.
    if(length(unzipped.rdml) > 1)
    {
      rdml.doc <- xmlParse(paste0(uniq.folder,"/rdml_data.xml"))
      dilutions.r <- GetDilutionsRoche(uniq.folder)
      
    }
    else
    {
      rdml.doc <- xmlParse(unzipped.rdml)
      #     private$.dilutions <- GetDilutions(rdml.doc)
    }},
    error = function(e) { print(e) },
    finally = unlink(uniq.folder, recursive = TRUE)
  )
  ####
  private$.dilutions <- dilutions.r
  rdml.root <- xmlRoot(rdml.doc)
  rdml.namespace <- c(rdml = "http://www.rdml.org")
  
  private$.dateMade <- xmlValue(rdml.root[["dateMade"]])
  
  private$.dateUpdated <- xmlValue(rdml.root[["dateUpdated"]])
  
  private$.id <- 
    llply(rdml.root["id"],
          function(id) 
            list(publisher = xmlValue(id[["publisher"]]),
                 serialNumber = xmlValue(id[["serialNumber"]]),
                 MD5Hash = xmlValue(id[["MD5Hash"]])
            )
    )
  
  
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
              list(
                id = xmlAttrs(sample, "id"),
                description = xmlValue(sample[["description"]]),
                documentation = sapply(sample["documentation"],
                                       function(documentation)
                                         if(!is.null(documentation))
                                           xmlAttrs(documentation, "id")
                ),
                xRef = ldply(sample["xRef"],
                             function(xRef) c(
                               name = xmlValue(xRef[["name"]]),
                               id = xmlValue(xRef[["id"]])
                             )),
                annotation = ldply(sample["annotation"],
                                   function(annotation) c(
                                     property = xmlValue(annotation[["property"]]),
                                     value = xmlValue(annotation[["value"]])
                                   )),
                type = type,
                interRunCalibrator = as.logical(xmlValue(sample[["interRunCalibrator"]])),
                quantity = 
                  list(value = as.numeric(xmlValue(sample[["quantity"]][["value"]])),
                       unit = xmlValue(sample[["quantity"]][["unit"]])),
                calibaratorSample = as.logical(xmlValue(sample[["calibaratorSample"]])),
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
  
  private$.target <- {
    target.list <- llply(rdml.root["target"],
                         function(target) { 
                           list(
                             id = xmlAttrs(target, "id"),
                             description = xmlValue(target[["description"]]),
                             documentation = sapply(target["documentation"],
                                                    function(documentation)
                                                      if(!is.null(documentation))
                                                        xmlAttrs(documentation, "id")
                             ),
                             xRef = ldply(target["xRef"],
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
  
  private$.thermalCyclingConditions <-{
    tcc.list <- 
      llply(rdml.root["thermalCyclingConditions"],
            function(tcc) {
              list(
                id = xmlAttrs(tcc, "id"),
                description = xmlValue(tcc[["description"]]),
                documentation = sapply(tcc["documentation"],
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
                               nr = xmlValue(step[["nr"]]),
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
      id = tar.id, # id==tar
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
      },
      endPt = as.numeric(xmlValue(data[["endPt"]])),
      bgFluor = as.numeric(xmlValue(data[["bgFluor"]])),
      bgFluorSp = as.numeric(xmlValue(data[["bgFluorSp"]])),
      quantFluor = as.numeric(xmlValue(data[["quantFluor"]]))
    )
  }
  
  GetReact <- function(react, experiment.id, run.id) {
    react.id <- xmlAttrs(react, "id")    
    react.id <- tryCatch(
      as.integer(react.id),
      warning = function(cond) {
        # if react.id is 'B1' not '13'
        # like in StepOne
        FromPositionToId(react.id)
      }    
    )    
    sample <- xmlAttrs(react[["sample"]],
                       "id")
    ######
    # remove Roche omitted ('ntp') samples
    if(is.null(private$.sample[[sample]]))
      return(NULL)
    #######
    list(
      id = react.id,
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
    list(
      id = xmlAttrs(run, "id"),
      description = xmlValue(run[["description"]]),
      documentation = sapply(run["documentation"],
                             function(documentation)
                               if(!is.null(documentation))
                                 xmlAttrs(documentation, "id")
      ),
      experimenter = sapply(run["experimenter"],
                            function(experimenter)
                              if(!is.null(experimenter))
                                xmlAttrs(experimenter, "id")
      ),
      instrument = xmlValue(run[["instrument"]]),
      dataCollectionSoftware = c(
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
                                         run.id)
          )
        names(react.list) <- GetIds(react.list)
        compact(react.list)                                
      }
    )
    
  }                      
  
  GetExperiment <- function(experiment) {
    experiment.id <- xmlAttrs(experiment, "id")            
    list(
      id = experiment.id,
      description = xmlValue(experiment[["description"]]),
      documentation = sapply(experiment["documentation"],
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
  
  if(private$.id[[1]]$publisher == "Roche Diagnostics") {
    for(conc.i in 1:length(dilutions.r[[1]])) {
      sample.id <- private$.experiment[[1]]$run[[1]]$react[[
        which(names(private$.experiment[[1]]$run[[1]]$react) == 
                names(dilutions.r[[1]])[conc.i])]]$sample      
      private$.sample[[sample.id]]$quantity <- list(
        value = dilutions.r[[1]][conc.i],
        unit = "other"
      )
    }
  }
}, 
overwrite = TRUE)