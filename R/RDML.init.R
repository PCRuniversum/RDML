rdml.env <- new.env(parent = emptyenv())

# XML parsing helpers -------------------------------------------------
getTextValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml_find_first(tree, path, ns)
  ifelse(class(node) == "xml_missing",
         return(NULL),
         xml_text(node))
}

getTextVector <- function(tree, path, ns = rdml.env$ns) {
  node.set <- xml_find_all(tree, path, ns)
  if (length(node.set) == 0)
    return(NULL)
  list.mapv(node.set,
            xml_text(.))
}

getLogicalValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml_find_first(tree, path, ns)
  ifelse(class(node) == "xml_missing",
         return(NULL),
         as.logical(xml_text(node)))
}

getNumericValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml_find_first(tree, path, ns)
  ifelse(class(node) == "xml_missing",
         return(NULL),
         as.numeric(xml_text(node)))
}

getNumericVector <- function(tree, path, ns = rdml.env$ns) {
  node.set <- xml_find_all(tree, path, ns)
  if (length(node.set) == 0)
    return(NULL)
  list.mapv(node.set,
            as.numeric(xml_text(.)))
}

getIntegerValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml_find_first(tree, path, ns)
  ifelse(class(node) == "xml_missing",
         return(NULL),
         as.integer(xml_text(node)))
}

genId <- function(node) {
  idType$new(xml_attr(node, "id"))
}

genIdRef <- function(node) {
  idReferencesType$new(xml_attr(node, "id"))
}

xmlValue <- function(val) {
  out <- XML::xmlValue(val)
  if (is.na(out))
    return(NULL)
  out
}

# xmlAttrs <- function(...) {
#   XML::xmlAttrs(...) %>>% iconv("UTF-8", "UTF-8")
# }

# as.logical <- function(val) {
#   out <- base::as.logical(val)
#   if (length(out) == 0)
#     return(NULL)
#   out
# }

as.numeric <- function(val) {
  out <- tryCatch(
    base::as.numeric(val),
    warning = function(w) {
      base::as.numeric(gsub(",", ".", val))
    }
  )
  if (length(out) == 0)
    return(NULL)
  out
}

as.integer <- function(val) {
  out <- base::as.integer(val)
  if (length(out) == 0)
    return(NULL)
  out
}

# Misc functions -------------------------------------------------
ns <- NULL

FromPositionToId <- function(react.id) {
  row <- which(LETTERS ==
                 gsub("([A-Z])[0-9]+", "\\1", react.id))
  col <- as.integer(gsub("[A-Z]([0-9]+)", "\\1", react.id))
  # as.character((row - 1) * 12 + col)
  (row - 1) * 12 + col
}

GetIds <- function(l) {
  unname(sapply(l, function(el) el$id))
}

# Gets concentrations (quantity) of each 
# dilution from XML for Roche
GetDilutionsRoche <- function(uniq.folder)
{
  # cat("\nParsing Roche standards data...")
  if(!file.exists(paste0(uniq.folder,"/calculated_data.xml"))) {
    # cat("NO SUCH FILE")
    return(NA)
  }
  rdml.doc <- read_xml(paste0(uniq.folder,"/calculated_data.xml"))
  xml_ns_strip(rdml.doc)
  concs <- getNumericVector(rdml.doc, "//absQuantDataSource/standard")
  concs.guids <- 
    getTextVector(rdml.doc, "//absQuantDataSource/standard/../graphId")
  if (length(concs) == 0) {
    concs <- getNumericVector(rdml.doc, "//relQuantDataSource/standard")
    concs.guids <- 
      getTextVector(rdml.doc, "//relQuantDataSource/standard/../graphId")
  }
  names(concs) <- concs.guids
  concs <- sort(concs, decreasing = TRUE)
  positions <- 
    getTextVector(rdml.doc, 
                  "//standardPoints/standardPoint/position")
  positions <- sapply(positions, FromPositionToId)
  dye.names <- getTextVector(rdml.doc, 
                             "//standardPoints/standardPoint/dyeName")
  positions.guids <- 
    getTextVector(rdml.doc, 
                  "//standardPoints/standardPoint/graphIds/guid")
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
  dilutions <- list.map(dyes, 
                        dye ~ {
                          dye.group.indecies <- which(positions.table["dye.name",] == dye)
                          concs.by.dye <- concs[dye.group.indecies]
                          names(concs.by.dye) <- positions.table["position",
                                                                 dye.group.indecies]
                          concs.by.dye
                        })
  if (length(dilutions) == 0) {
    return(NULL)
  }
  names(dilutions) <- dyes
  return(dilutions)
}

GetConditionsRoche <- function(uniq.folder)
{
  # cat("\nParsing Roche conditions data...")
  if (!file.exists(paste0(uniq.folder, "/app_data.xml"))) {
    # cat("NO SUCH FILE")
    return(NA)
  }
  rdml.doc <- read_xml(paste0(uniq.folder, "/app_data.xml"))
  xml_ns_strip(rdml.doc)
  nodes <- xml_find_all(rdml.doc,
                      "/rocheLC96AppExtension/experiment/run/react/condition/..")
  reacts <- xml_attr(nodes, "id")
  conditions <- getTextVector(nodes, "./condition")
  if (length(conditions) == 0) {
    # cat("NONE")
    return(NULL)
  }
  names(conditions) <- reacts
  # cat("OK")
  return(conditions)
}

GetRefGenesRoche <- function(uniq.folder)
{
  # cat("\nParsing Roche reference genes data...")
  if (!file.exists(paste0(uniq.folder, "/module_data.xml"))) {
    # cat("NO SUCH FILE")
    return(NA)
  }
  rdml.doc <- read_xml(paste0(uniq.folder, "/module_data.xml"))
  ref <- xml_find_all(rdml.doc,
                      "//d3:geneSettings/d3:relQuantGeneSettings")
    # namespaces = c(ns = "http://www.roche.ch/LC96RelQuantGeneralDataModel"))
  
  if (length(ref) == 0) {
    # cat("NONE")
    return(NULL)
  }
  xml_ns_strip(ref)
  return(ref)
}

# RDML init -------------------------------------------------
#' Creates new instance of \code{RDML} class object
#' 
#' This function has been designed to import data from RDML v1.1 and v1.2 format
#' files or from \code{xls} file generated by \emph{Applied Biosystems 7500}. To
#' import from \code{xls} this file have to contain \code{Sample Setup} and 
#' \code{Multicomponent Data} sheets!
#' 
#' File format options: \describe{\item{auto}{Tries to detect format by
#' extension. \code{.xlsx} -- \code{excel}, \code{.xls} -- \code{abi}, 
#' \code{.csv} -- \code{csv}, other -- \code{rdml}}\item{abi}{Reads \code{.xls}
#' files generated by \emph{ABI 7500 v.2}. To create such files use File>Export;
#' check 'Sample Setup' and 'Multicomponent Data'; select 'One File'}
#' \item{excel}{\code{.xls} or \code{.xslx} file with sheets 'description', 
#' 'adp', 'mdp'. See example file \code{table.xlsx}}\item{csv}{\code{.csv} file 
#' with first column 'cyc' or 'tmp' and fluorescence data in other columns}
#' \item{rdml}{\code{.rdml} or \code{.lc96p} files}}
#' 
#' 
#' @section Warning: Although the format RDML claimed as data exchange format, 
#'   the specific implementation of the format at devices from real 
#'   manufacturers differ significantly. Currently this function is checked 
#'   against RDML data from devices: \emph{Bio-Rad CFX96}, \emph{Roche 
#'   LightCycler 96} and \emph{Applied Biosystems StepOne}.
#' @param filename \code{string} -- path to file
#' @param show.progress \code{logical} -- show loading progress bar if 
#'   \code{TRUE}
#' @param conditions.sep separator for condition defined at sample name
#' @param format \code{string} -- input file format. Possible values 
#'   \code{auto}, \code{rdml}, \code{abi}, \code{excel}, \code{csv}. See 
#'   Details.
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>, Stefan Roediger 
#'   <stefan.roediger@@hs-lausitz.de>, Michal Burdukiewicz 
#'   <michalburdukiewicz@@gmail.com>
#' @docType methods
#' @name new
#' @aliases RDML.new
#' @rdname new-method
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom stringr str_match_all
#' @include RDML.R
#' @examples
#' \dontrun{
#' ## Import from RDML file
#' PATH <- path.package("RDML")
#' filename <- paste(PATH, "/extdata/", "lc96_bACTXY.rdml", sep ="")
#' lc96 <- RDML$new(filename)
#' 
#' ## Some kind of overview for lc96
#' lc96$AsTable(name.pattern = sample[[react$sample$id]]$description)
#' lc96$AsDendrogram()
#' }
RDML$set("public", "initialize", function(filename,
                                          show.progress = TRUE,
                                          conditions.sep = NULL,
                                          format = "auto" #,
                                          # dateMade = NULL,
                                          # dateUpdated = NULL,
                                          # id = NULL,
                                          # experimenter = NULL,
                                          # documentation = NULL,
                                          # dye = NULL,
                                          # sample = NULL,
                                          # target = NULL,
                                          # thermalCyclingConditions = NULL,
                                          # experiment = NULL
) {  
  if(missing(filename)) {
    return()
  }
  assertString(filename)
  
  # ABI7500 -----------------------------------------------------------------
  fromABI <- function() {
    tryCatch({
      uniq.folder <- tempfile()
      unzipped <- unzip(filename, exdir = uniq.folder)
      # multicomponent.data <- paste0(uniq.folder, "\\apldbio\\sds\\multicomponent_data.txt") %>>% 
      #   read.delim(skip = 2, stringsAsFactors = FALSE) %>>% 
      #   filter(!is.na(WELL))
      data.file <- paste0(uniq.folder, "\\apldbio\\sds\\multicomponent_data.txt")
      multicomponent.data <- readChar(data.file, file.info(data.file)$size) %>>% 
        str_match_all("([0-9]+)\\t([0-9]+)\\t([A-Z]+)\\t[0-9]+\\.?[0-9]*\\t([0-9]+\\.?[0-9]*)")
      multicomponent.data <- as.data.frame(multicomponent.data[[1]], stringsAsFactors = FALSE)
      names(multicomponent.data) <- c("_", "well", "cyc", "dye", "fluor")
      multicomponent.data <- multicomponent.data %>>% 
        mutate(well = as.numeric(well),
               cyc = as.numeric(cyc),
               fluor = as.numeric(fluor))
      ncycles <- multicomponent.data$cyc %>>% max + 1
      
      plate.setup <- paste0(uniq.folder, "\\apldbio\\sds\\plate_setup.xml") %>>%
        xmlParse() %>>% xmlRoot()
      
      snames <- xpathSApply(plate.setup,
                            "/Plate/FeatureMap/Feature[Id='sample']/../FeatureValue/FeatureItem/Sample/Name",
                            xmlValue)
      names(snames) <- xpathSApply(plate.setup,
                                   "/Plate/FeatureMap/Feature[Id='sample']/../FeatureValue/Index",
                                   xmlValue) %>>% as.integer + 1
      
      description <- data.frame()
      list.iter(getNodeSet(plate.setup,
                           "/Plate/FeatureMap/Feature[Id='detector-task']/../FeatureValue"), {
                             index <- .[["Index"]] %>>% xmlValue() %>>% as.integer + 1
                             
                             #only one task allowed!
                             task <- .[["FeatureItem"]][["DetectorTaskList"]][[1]][["Task"]] %>>%
                               xmlValue %>>% 
                               switch (
                                 UNKNOWN = "unkn",
                                 NTC = "ntc",
                                 STANDARD = "std"
                               )
                             size <- .[["FeatureItem"]][["DetectorTaskList"]] %>>% xmlSize
                             list.iter(.[["FeatureItem"]][["DetectorTaskList"]][1:size], {
                               description <<- rbind(description, 
                                                     data.frame(fdata.name = paste(index, .[["Detector"]][["Reporter"]] %>>% xmlValue),
                                                                exp.id = "exp1",
                                                                run.id = "run1",
                                                                react.id = index,
                                                                sample = ifelse(is.na(snames[as.character(index)]),
                                                                                "unnamed", snames[as.character(index)]) %>>% unname,
                                                                type = task,
                                                                target = .[["Detector"]][["Name"]] %>>% xmlValue,
                                                                target.dyeId = .[["Detector"]][["Reporter"]] %>>% xmlValue,
                                                                quantity = .[["Concentration"]] %>>% xmlValue %>>% as.numeric,
                                                                IsOmit = FALSE,
                                                                stringsAsFactors = FALSE))
                             })})
      
      
      omitted.i <- xpathSApply(plate.setup,
                               "/Plate/Wells/Well[IsOmit='true']/Index",
                               xmlValue) %>>% as.integer + 1
      description[description$react.id == omitted.i, "IsOmit"] <- TRUE
      description <- description %>>%
        filter(IsOmit == FALSE)
      
      fdata <- cbind(data.frame(multicomponent.data$cyc %>>% unique + 1),
                     apply(description, 1, 
                           function(r) {
                             multicomponent.data %>>% filter(well == as.integer(r["react.id"]) - 1,
                                                            dye == r["target.dyeId"]) %>>% 
                               .$fluor
                           }))
      names(fdata) <- c("cyc", description$fdata.name)
      self$SetFData(fdata, description)
      self$id <- list(rdmlIdType$new("ABI" , "1"))
    },
    error = function(e) { stop(e) },
    finally = unlink(uniq.folder, recursive = TRUE))
  }
  
  # RotorGene -----------------------------------------------------------------
  fromRotorGene <- function() {
    dat <- filename %>>% 
      xmlParse %>>% xmlRoot
    
    description <- dat %>>% 
      getNodeSet("/Experiment/Samples/Page/Sample/Name/text()[1]/../..") %>>% 
      list.map(xmlToList(.)) %>>% 
      list.map(list(
        fdata.name = ID,
        exp.id = "exp1",
        run.id = "run1",
        react.id = TubePosition,
        sample = Name,
        type = switch(Type,
                      "5" = "pos",
                      "3" = "ntc",
                      "1" = "std",
                      "unkn"),
        quantity = GivenConc
      ))  %>>% 
      list.stack %>>% 
      mutate(react.id = react.id %>>% as.numeric,
             quantity = quantity %>>% as.numeric)
    
    dat[["Samples"]][["Groups"]] %>>% 
      xmlToList %>>% 
      list.iter(group ~ {
        ids <- group[-c(1, 2)] %>>% list.mapv(Tube)
        description[description$fdata.name %in% ids, "target"] <<- group$Name
      })
    
    original.targets <- description$target
    dat %>>% 
      getNodeSet("/Experiment/RawChannels/RawChannel") %>>% 
      list.iter(rawChannel ~ {
        description$target.dyeId <<- rawChannel[["Name"]] %>>% xmlValue
        description$target <<- paste(original.targets,
                                     description$target.dyeId[1], sep = "#")
        fdata <- 
          xpathApply(rawChannel,
                     sprintf(
                     "/Experiment/RawChannels/RawChannel/Name[text()='%s']/../Reading",
                     description$target.dyeId[1]
                     ),
                     xmlValue)[description$react.id] %>>%
          list.map(x ~ {strsplit(x, " ") %>>% .[[1]] %>>% as.numeric %>>% as.list}) %>>%
          list.stack %>>% 
          t
        colnames(fdata) <- description$react.id
        fdata <- cbind(cyc = 1:nrow(fdata), fdata)
        self$SetFData(fdata, description)
      })
    self$id <- list(rdmlIdType$new("RotorGene" , "1"))
  }
  
  # From Excel -----------------------------------------------------------------
  fromExcel <- function() {
    descr <- read_excel(filename,
                        sheet = "description")
    adp_data <- tryCatch({
      read_excel(filename,
                 sheet = "adp") %>>% 
        sapply(as.numeric) %>>% 
        as.data.frame
    },
    error = function(e) NULL)
    
    mdp_data <- tryCatch({
      read_excel(filename,
                 sheet = "mdp") %>>% 
        sapply(as.numeric) %>>% 
        as.data.frame
    },
    error = function(e) NULL)
    
    if (!is.null(adp_data))
      self$SetFData(adp_data, descr)
    if (!is.null(mdp_data))
      self$SetFData(mdp_data, descr, fdata.type = "mdp")
  }
  
  # From CSV -----------------------------------------------------------------
  fromCSV <- function() {
    pcrdata <- read.csv(filename)
    fdata.names <- colnames(pcrdata)[-1]
    data.type <- {
      if (colnames(pcrdata)[1] == "cyc")
        "adp"
      else
        "mdp"
    }
    descr <- data.frame(
      fdata.name  = fdata.names,
      exp.id = "exp1",
      run.id = "run1",
      react.id = 1:length(fdata.names),
      sample = fdata.names,
      target = "unkn",
      target.dyeId = "unkn",
      type = "unkn",
      stringsAsFactors = FALSE
    )
    self$SetFData(pcrdata, descr, fdata.type = data.type)
  }
  
  # From RDML, lc96 -----------------------------------------------------------------
  fromRDML <- function() {
    rdml.env$ns <- NULL
    # Unzips RDML to unique folder to get inner XML content.
    # Unique folder is needed to prevent file ovewriting
    # by parallel function usage.
    uniq.folder <- tempfile() #paste0(tempdir(), UUIDgenerate())
    # cat(sprintf("Unzipping %s...", filename))
    unzipped.rdml <- unzip(filename, exdir = uniq.folder)
    dilutions.r <- NULL
    ref.genes.r <- NULL
    
    tryCatch({
      # Roche use more than one file at RDML zip.
      # One of the files store dilutions information.
      if (length(unzipped.rdml) > 1)
      {
        # cat("\nParsing Roche(?) data...")
        rdml.doc <- read_xml(paste0(uniq.folder,"/rdml_data.xml"))
        rdml.env$ns <- xml_ns_rename(xml_ns(rdml.doc), "d1" = "rdml")
        dilutions.r <- GetDilutionsRoche(uniq.folder)
        conditions.r <- GetConditionsRoche(uniq.folder)
        ref.genes.r <- GetRefGenesRoche(uniq.folder)
        # private$.dilutions <- dilutions.r
      }
      else
      {
        # cat("\nParsing data...")
        rdml.doc <- read_xml(unzipped.rdml)
        rdml.env$ns <- xml_ns(rdml.doc)
        #     private$.dilutions <- GetDilutions(rdml.doc)
      }},
      error = function(e) { stop(e) },
      finally = unlink(uniq.folder, recursive = TRUE)
    )
    
    # dateMade -----------------------------------------------------------------
    # cat("\nGetting dateMade")
    private$.dateMade <- getTextValue(rdml.doc, "/rdml:rdml/rdml:dateMade")
    
    # dateUpdated -----------------------------------------------------------------
    # cat("\nGetting dateUpdated")
    private$.dateUpdated <- getTextValue(rdml.doc, "/rdml:rdml/rdml:dateUpdated")
    
    # id -----------------------------------------------------------------
    # cat("\nGetting id")
    private$.id <- 
      list.map(rdml.doc %>>% xml_find_all("/rdml:rdml/rdml:id", rdml.env$ns),
               id ~ 
                 rdmlIdType$new(
                   publisher = getTextValue(tree = id, path = "./rdml:publisher"),
                   serialNumber = getTextValue(tree = id, path = "./rdml:serialNumber"),
                   MD5Hash = getTextValue(tree = id, path = "./rdml:MD5Hash")
                 )
      ) %>>%  
      with.names(quote(.$publisher))
    # cat("\nGetting experementer")
    private$.experimenter <- {
      # experimenter.list <- 
      list.map(rdml.doc %>>% 
                 xml_find_all("/rdml:rdml/rdml:experimenter", rdml.env$ns),
               experimenter ~
                 experimenterType$new(
                   id = genId(experimenter),
                   firstName = getTextValue(experimenter, "./rdml:firstName"),
                   lastName = getTextValue(experimenter, "./rdml:lastName"),
                   email = getTextValue(experimenter, "./rdml:email"),
                   labName = getTextValue(experimenter, "./rdml:labName"),
                   labAddress = getTextValue(experimenter, "./rdml:labAddress")
                 )) %>>% 
        with.names(quote(.$id$id))
    }
    
    # documentation -----------------------------------------------------------------
    # cat("\nGetting documentation")
    private$.documentation <- {
      # documentation.list <- 
      list.map(rdml.doc %>>% 
                 xml_find_all("/rdml:rdml/rdml:documentation", rdml.env$ns),
               documentation ~
                 documentationType$new(
                   id = genId(documentation),
                   text = getTextValue(documentation, "./rdml:text")
                 )) %>>% 
        with.names(quote(.$id$id))
    }
    
    # dye -----------------------------------------------------------------
    # cat("\nGetting dye")
    private$.dye <- {
      list.map(rdml.doc %>>% 
                 xml_find_all("/rdml:rdml/rdml:dye", rdml.env$ns),
               dye ~ dyeType$new(
                 id = genId(dye),
                 description = getTextValue(dye, "./rdml:description")
               )) %>>% 
        with.names(quote(.$id$id))
    }
    
    # sample -----------------------------------------------------------------
    # cat("\nGetting sample")
    private$.sample <- 
      list.map(rdml.doc %>>% 
                 xml_find_all("/rdml:rdml/rdml:sample", rdml.env$ns),
               sample ~ 
               {
                 type <- getTextValue(sample, "./rdml:type")
                 
                 # remove Roche omitted ('ntp') samples
                 if(type == "ntp")
                   return(NULL)
                 
                 # id <- xmlAttrs(sample, "id")
                 sampleType$new(
                   id = genId(sample),
                   description = getTextValue(sample, "./rdml:description"),
                   documentation = 
                     list.map(sample %>>% 
                                xml_find_all("./rdml:documentation", rdml.env$ns),
                              doc ~ genIdRef(doc)),
                   xRef = 
                     list.map(sample %>>% 
                                xml_find_all("./rdml:xRef", rdml.env$ns),
                              xRef ~ xRefType$new(
                                name = getTextValue(xRef, "./rdml:name"),
                                id = getTextValue(xRef, "./rdml:id")
                              )),
                   annotation = c(
                     list.map(sample %>>% 
                                xml_find_all("./rdml:annotation", rdml.env$ns),
                              annotation ~ annotationType$new(
                               property = getTextValue(annotation, "./rdml:property"),
                               value = getTextValue(annotation, "./rdml:value")
                              )),                  
                     if (!is.null(conditions.sep)) {
                       val <- gsub(sprintf("^.*%s(.*)$",
                                           conditions.sep),
                                   "\\1", id)
                       if (length(val) != 0) {
                         annotationType$new(
                           property = "condition",
                           value = val)
                       }
                     }),
                   type = sampleTypeType$new(type),
                   interRunCalibrator = 
                     getLogicalValue(sample, "./rdml:interRunCalibrator"),
                   quantity = 
                     tryCatch(
                       quantityType$new(
                         value = getNumericValue(sample, "./rdml:quantity/rdml:value"),
                         unit = quantityUnitType$new(
                           getTextValue(sample, "./rdml:quantity/rdml:unit"))),
                       error = function(e) NULL
                     ),
                   calibratorSample = 
                     getLogicalValue(sample, "./rdml:calibaratorSample"),
                   cdnaSynthesisMethod = 
                     cdnaSynthesisMethodType$new(
                       enzyme = getTextValue(sample, "./rdml:cdnaSynthesisMethod/rdml:enzyme"),
                       primingMethod =
                         primingMethodType$new(getTextValue(sample, 
                                                            "./rdml:cdnaSynthesisMethod/rdml:primingMethod")),
                       dnaseTreatment = getLogicalValue(sample, "./rdml:cdnaSynthesisMethod/rdml:dnaseTreatment"),
                       thermalCyclingConditions = 
                         tryCatch(
                           genIdRef(xml_find_first(sample, "./rdml:cdnaSynthesisMethod/rdml:thermalCyclingConditions")),
                           error = function(e) NULL)
                     ),
                   templateQuantity =
                     tryCatch(
                       templateQuantityType$new(
                         conc = getNumericValue(sample, "./rdml:templateQuantity/rdml:conc"),
                         nucleotide = nucleotideType$new(
                           getTextValue(sample, "./rdml:templateQuantity/rdml:nucleotide"))
                       ),
                       error = function(e) NULL
                     )
                 )
               }) %>>% 
      list.filter(!is.null(.)) %>>% 
      with.names(quote(.$id$id))
    
    # target -----------------------------------------------------------------
    # cat("\nGetting target")
    private$.target <- 
      list.map(rdml.doc %>>% 
                 xml_find_all("/rdml:rdml/rdml:target", rdml.env$ns),
            target ~ {
              targetType$new(
                id = xml_attr(target, "id") %>>% 
                  (id ~ idType$new( 
                    ifelse(length(unzipped.rdml) > 1 &&
                             length(private$.id) != 0 &&
                             private$.id[[1]]$publisher == "Roche Diagnostics",
                           {
                             gsub("@(.+)$", "\\1", 
                                  regmatches(id, gregexpr("@(.+)$", id))[[1]])
                           },
                           id))),
                description = getTextValue(target, "./rdml:description"),
                documentation = 
                  list.map(target %>>% 
                             xml_find_all("./rdml:documentation", rdml.env$ns),
                        doc ~ genIdRef(doc)
                  ),
                xRef = 
                  list.map(target %>>% 
                             xml_find_all("./rdml:xRef", rdml.env$ns),
                           xRef ~
                          xRefType$new(
                            name = getTextValue(xRef, "./rdml:name"),
                            id = getTextValue(xRef, "./rdml:id")
                          )),
                type = targetTypeType$new(getTextValue(target, "./rdml:type")),
                amplificationEfficiencyMethod = 
                  getTextValue(target, "./rdml:amplificationEfficiencyMethod"),
                amplificationEfficiency = 
                  getNumericValue(target, "./rdml:amplificationEfficiency"),
                amplificationEfficiencySE = 
                  getNumericValue(target, "./rdml:amplificationEfficiencySE"),
                detectionLimit = 
                  getNumericValue(target, "./rdml:detectionLimit"),
                dyeId =
                  tryCatch(
                    target %>>% 
                      xml_find_first("./rdml:dyeId", rdml.env$ns) %>>% 
                      genIdRef(),
                    # StepOne stores dyeId as xmlValue 
                    error = function(e)
                      idReferencesType$new(getTextValue(target, "./rdml:dyeId"))
                  ),
                # dyeId = NA,
                
                sequences = sequencesType$new(
                  forwardPrimer = 
                    tryCatch(
                      oligoType$new(
                        threePrimeTag = 
                          getTextValue(target, "./rdml:sequences/rdml:forwardPrimer/rdml:threePrimeTag"),
                        fivePrimeTag = 
                          getTextValue(target, "./rdml:sequences/rdml:forwardPrimer/rdml:fivePrimeTag"),
                        sequence = 
                          getTextValue(target, "./rdml:sequences/rdml:forwardPrimer/rdml:sequence")),
                      error = function(e) NULL
                    ),
                  reversePrimer = 
                    tryCatch(
                      oligoType$new(
                        threePrimeTag = 
                          getTextValue(target, "./rdml:sequences/rdml:reversePrimer/rdml:threePrimeTag"),
                        fivePrimeTag = 
                          getTextValue(target, "./rdml:sequences/rdml:reversePrimer/rdml:fivePrimeTag"),
                        sequence = 
                          getTextValue(target, "./rdml:sequences/rdml:reversePrimer/rdml:sequence")),
                      error = function(e) NULL
                    ),
                  probe1 =
                    tryCatch(
                      oligoType$new(
                        threePrimeTag = 
                          getTextValue(target, "./rdml:sequences/rdml:probe1/rdml:threePrimeTag"),
                        fivePrimeTag = 
                          getTextValue(target, "./rdml:sequences/rdml:probe1/rdml:fivePrimeTag"),
                        sequence = 
                          getTextValue(target, "./rdml:sequences/rdml:probe1/rdml:sequence")),
                      error = function(e) NULL
                    ),
                  probe2 = 
                    tryCatch(
                    oligoType$new(
                      threePrimeTag = 
                        getTextValue(target, "./rdml:sequences/rdml:probe2/rdml:threePrimeTag"),
                      fivePrimeTag = 
                        getTextValue(target, "./rdml:sequences/rdml:probe2/rdml:fivePrimeTag"),
                      sequence = 
                        getTextValue(target, "./rdml:sequences/rdml:probe2/rdml:sequence")),
                    error = function(e) NULL
                  ),
                  amplicon = 
                    tryCatch(
                      oligoType$new(
                        threePrimeTag = 
                          getTextValue(target, "./rdml:sequences/rdml:amplicon/rdml:threePrimeTag"),
                        fivePrimeTag = 
                          getTextValue(target, "./rdml:sequences/rdml:amplicon/rdml:fivePrimeTag"),
                        sequence = 
                          getTextValue(target, "./rdml:sequences/rdml:amplicon/rdml:sequence")),
                      error = function(e) NULL
                    )),
                commercialAssay = 
                  tryCatch(
                    commercialAssayType$new(
                      company = 
                        getTextValue(target, "./rdml:commercialAssay/rdml:company"),
                      orderNumber = 
                        getTextValue(target, "./rdml:commercialAssay/rdml:orderNumber"))
                    ,
                    error = function(e) NULL
                  )
              )
            }
      ) %>>% 
      with.names(quote(.$id$id))
    
    # thermalCyclingConditions -------------------------------------------------
    # cat("\nGetting thermalCyclingConditions")
    private$.thermalCyclingConditions <- 
      list.map(rdml.doc %>>% 
                 xml_find_all("/rdml:rdml/rdml:thermalCyclingConditions", rdml.env$ns),
               tcc ~ {
              thermalCyclingConditionsType$new(
                id = genId(tcc),
                description = getTextValue(tcc, "./rdml:description"),
                documentation = 
                  list.map(tcc %>>% 
                             xml_find_all("./rdml:documentation", rdml.env$ns),
                           doc ~ genIdRef(doc)
                  ),
                lidTemperature = 
                  getNumericValue(tcc, "./rdml:lidTemperature"),
                
                experimenter = 
                  list.map(tcc %>>% 
                             xml_find_all("./rdml:experimenter", rdml.env$ns),
                           experimenter ~ genIdRef(experimenter)
                  ),
                
                step = list.map(tcc %>>% 
                                  xml_find_all("./rdml:step", rdml.env$ns),
                             step ~ {
                               stepType$new(
                                 nr = getIntegerValue(step, "./rdml:nr"),
                                 description = getTextValue(step, "./rdml:description"),
                                 temperature = {
                                   tryCatch(
                                     temperatureType$new(
                                       temperature = 
                                         getNumericValue(step, "./rdml:temperature/rdml:temperature"),
                                       duration = 
                                         getIntegerValue(step, "./rdml:temperature/rdml:duration"),
                                       temperatureChange = 
                                         getNumericValue(step, "./rdml:temperature/rdml:temperatureChange"),
                                       durationChange = 
                                         getIntegerValue(step, "./rdml:temperature/rdml:durationChange"),
                                       measure = measureType$new(
                                         getTextValue(step, "./rdml:temperature/rdml:measure")),
                                       ramp = 
                                         getNumericValue(step, "./rdml:temperature/rdml:ramp")
                                     ),
                                     error = function(e) NULL)},
                                 gradient = {
                                   tryCatch(
                                     gradientType$new(
                                       highTemperature = 
                                         getNumericValue(step, "./rdml:gradient/rdml:highTemperature"),
                                       lowTemperature = 
                                         getNumericValue(step, "./rdml:gradient/rdml:lowTemperature"),
                                       duration = 
                                         getIntegerValue(step, "./rdml:gradient/rdml:duration"),
                                       temperatureChange = 
                                         getNumericValue(step, "./rdml:gradient/rdml:temperatureChange"),
                                       durationChange = 
                                         getIntegerValue(step, "./rdml:gradient/rdml:durationChange"),
                                       measure = measureType$new(
                                         getTextValue(step, "./rdml:gradient/rdml:measure")),
                                       ramp = 
                                         getNumericValue(step, "./rdml:gradient/rdml:ramp")), 
                                   error = function(e) NULL)
                                 },
                                 loop = {
                                   tryCatch(
                                     loopType$new(
                                       goto = getIntegerValue(step, "./rdml:loop/rdml:goto"),
                                       # should be called "repeat" but this is reserved word
                                       repeat.n = getIntegerValue(step, "./rdml:loop/rdml:repeat")),
                                     error = function(e) NULL)},
                                 pause = {
                                   tryCatch(
                                     pauseType$new(
                                       temperature = 
                                         getNumericValue(step, "./rdml:pause/rdml:temperature")),
                                     error = function(e) NULL)},
                                 lidOpen = {
                                   if(is.null(step[["lidOpen"]]))
                                     NULL
                                   else
                                     lidOpenType$new()
                                 }
                               )
                             }
                )
              )
            }) %>>% 
      with.names(quote(.$id$id))
    #     names(tcc.list) <- GetIds(tcc.list)
    #     tcc.list
    
    # data -------------------------------------------------
    GetData <- function(data, experiment.id, run.id, react.id) {
      tar.id <- 
        data %>>%
        xml_find_first("./rdml:tar", rdml.env$ns) %>>%
        xml_attr("id")
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
      dataType$new(
        tar = idReferencesType$new(
          ifelse(length(unzipped.rdml) > 1 &&
                   length(private$.id) != 0 &&
                   private$.id[[1]]$publisher == "Roche Diagnostics",
                 gsub("@(.+)$", "\\1", 
                      regmatches(tar.id, gregexpr("@(.+)$", tar.id))[[1]])
                 ,
                 tar.id)), 
        cq = getNumericValue(data, "./rdml:cq"),
        excl = getTextValue(data, "./rdml:excl"),
        adp = {
          cyc <- getNumericVector(rdml.doc,paste0(data.req, "/rdml:adp/rdml:cyc"))
          tmp <- getNumericVector(rdml.doc,paste0(data.req, "/rdml:adp/rdml:tmp"))                                          
          fluor <- getNumericVector(rdml.doc,paste0(data.req, "/rdml:adp/rdml:fluor"))
          if (!is.null(fluor)) {
            if (!is.null(tmp)) {
              # tryCatch(
                adpsType$new(
                  data.table(cyc = cyc, tmp = tmp, fluor = fluor))
              # ,
              #   warning = function(w) {
              #     dat <<- list(cyc, tmp, fluor)
              #     stop("warn")
              #   }
              # )
            } else {
              adpsType$new(
                data.table(cyc = cyc, fluor = fluor))
            }
          } else {
            #           matrix(ncol = 2,
            #                  dimnames = list(NULL,
            #                                  c("cyc", "tmp", "fluor")))
            NULL
          }
        },
        mdp = {                                                             
          tmp <- getNumericVector(rdml.doc,paste0(data.req, "/rdml:mdp/rdml:tmp"))                                          
          fluor <- getNumericVector(rdml.doc,paste0(data.req, "/rdml:mdp/rdml:fluor"))
          if (length(fluor) != 0 && !is.null(fluor)) {
            #           matrix(c(tmp, fluor), 
            #                                                byrow = FALSE,
            #                                                ncol = 2,
            #                                                dimnames = list(NULL,
            #                                                                c("tmp", "fluor"))) %>>% 
            #             typeof %>>% print
            #           NULL
            mdpsType$new(data.table(tmp = tmp, fluor = fluor))
          } else {
            #           matrix(ncol = 2,
            #                  dimnames = list(NULL,
            #                                  c("tmp", "fluor")))
            NULL
          }
        },
        endPt = getNumericValue(data, "./rdml:endPt"),
        bgFluor = getNumericValue(data, "./rdml:bgFluor"),
        bgFluorSlp = getNumericValue(data, "./rdml:bgFluorSp"),
        quantFluor = getNumericValue(data, "./rdml:quantFluor")
      )
    }
    
    # react -------------------------------------------------
    GetReact <- function(react, experiment.id, run.id) {
      react.id <- xml_attr(react, "id")    
      react.id.corrected <- tryCatch(
        as.integer(react.id),
        warning = function(w) {
          # if react.id is 'B1' not '13'
          # like in StepOne
          FromPositionToId(react.id)
        }    
      )
      #     cat(sprintf("\nreact: %i", react.id))
      sample <- 
        react %>>% 
        xml_find_first("./rdml:sample", rdml.env$ns) %>>% 
        xml_attr("id")
      
      if(length(unzipped.rdml) > 1 &&
         length(private$.id) != 0 && 
         private$.id[[1]]$publisher == "Roche Diagnostics") {
        # remove Roche omitted ('ntp') samples
        if(is.null(private$.sample[[sample]]))
          return(NULL)
        # Better names for Roche    
        sample <- private$.sample[[sample]]$description
      }
      
      reactType$new(
        id = reactIdType$new(react.id.corrected), #sample.id
        #       # will be calculated at the end of init
        #       position = NA,
        sample = idReferencesType$new(sample),
        data = {
          list.map(react %>>% 
                     xml_find_all("./rdml:data", rdml.env$ns),
                   data ~ GetData(data,
                                  experiment.id,
                                  run.id,
                                  react.id)
          ) 
        }
      )
    }  
    
    # run -------------------------------------------------
    GetRun <- function(run, experiment.id) {
      run.id <- xml_attr(run, "id")
      pcrFormat <- {
        rows <- getIntegerValue(run, "./rdml:pcrFormat/rdml:rows")
        # check for absent of pcrFormat
        # like in StepOne
        if (!is.null(rows) && !is.na(rows)) {
          pcrFormatType$new(
            rows = rows,
            columns = getIntegerValue(run, "./rdml:pcrFormat/rdml:columns"),
            rowLabel = labelFormatType$new(
              getTextValue(run, "./rdml:pcrFormat/rdml:rowLabel")),
            columnLabel = labelFormatType$new(
              getTextValue(run, "./rdml:pcrFormat/rdml:columnLabel"))
          )
        } else {
          pcrFormatType$new(
            rows = 8,
            columns = 12,
            rowLabel = labelFormatType$new("ABC"),
            columnLabel = labelFormatType$new("123")
          )
        }
      }
      if (show.progress)
        cat(sprintf("\n\trun: %s\n", run.id))
      runType$new(
        id = idType$new(run.id), #xmlAttrs(run, "id"),
        description = getTextValue(run, "./rdml:description"),
        documentation = 
          list.map(run %>>% 
                     xml_find_all("./rdml:documentation", rdml.env$ns),
                   doc ~ genIdRef(doc)
          ),
        experimenter = 
          list.map(run %>>% 
                     xml_find_all("./rdml:experimenter", rdml.env$ns),
                   experimenter ~ genIdRef(experimenter)
          ),
        instrument = getTextValue(run, "./rdml:instrument"),
        dataCollectionSoftware = 
          tryCatch(
            dataCollectionSoftwareType$new(
              name = getTextValue(run, "./rdml:dataCollectionSoftware/rdml:name"),
              version = getTextValue(run, "./rdml:dataCollectionSoftware/rdml:version")
            ),
            error = function(e) NULL),
        backgroundDeterminationMethod = 
          getTextValue(run, "./rdml:backgroundDeterminationMethod"),
        cqDetectionMethod = 
          cqDetectionMethodType$new(getTextValue(run, "./rdml:cqDetectionMethod")),
        thermalCyclingConditions = 
          tryCatch(
            run %>>%
              xml_find_first("rdml:thermalCyclingConditions", rdml.env$ns) %>>% 
              genIdRef(),
            error = function(e) NULL),
        pcrFormat = pcrFormat,
        runDate = getTextValue(run, "./rdml:runDate"),
        react =
          list.map(run %>>%
                     xml_find_all("./rdml:react", rdml.env$ns),
                   react ~ GetReact(react, 
                                    experiment.id,
                                    run.id)) %>>% 
          list.filter(!is.null(.))
      )
      
    }                      
    
    # experiment -------------------------------------------------
    GetExperiment <- function(experiment) {
      experiment.id <- xml_attr(experiment, "id")
      if (show.progress)
        cat(sprintf("\nLoading experiment: %s", experiment.id))
      experimentType$new(
        id = idType$new(experiment.id),
        description = getTextValue(experiment, "./rdml:description"),
        documentation = 
          list.map(experiment %>>% 
                     xml_find_all("./rdml:documentation", rdml.env$ns),
                   doc ~ genIdRef(doc)
          ),
        run = 
          list.map(experiment %>>% 
                     xml_find_all("./rdml:run", rdml.env$ns),
                run ~ GetRun(run, experiment.id)
          )
      )
    }
    
    
    private$.experiment <- 
      list.map(rdml.doc %>>% 
                 xml_find_all("./rdml:experiment", rdml.env$ns),
               experiment ~ GetExperiment(experiment)
      ) %>>% 
      list.names(.$id$id)
    
    # return()
    # private$.recalcPositions()
    
    # Roche LC96 extra parsing -------------------------------------------------
    # parse original!!! Roche files
    if (length(unzipped.rdml) > 1 &&
        length(private$.id) != 0 && 
        private$.id[[1]]$publisher == "Roche Diagnostics") {    
      for(i in 1:length(private$.sample)) {
        private$.sample[[i]]$id <- idType$new(private$.sample[[i]]$description)
      }
      private$.sample <- with.names(private$.sample,
                                    quote(.$id$id))
      
      # cat("Adding Roche ref genes\n")
      if (!is.null(ref.genes.r) &&
          !is.na(ref.genes.r) &&
          length(ref.genes.r) != 0) {
        list.iter(ref.genes.r,
                  ref.gene ~ {
                    geneName <- getTextValue(ref.gene, "./geneName")
                    geneI <- grep(
                      sprintf("^%s$", geneName),
                      names(private$.target))
                    private$.target[[geneI]]$type <-
                      targetTypeType$new(
                        ifelse(getLogicalValue(ref.gene, "./isReference"),
                               "ref",
                               "toi"))
                  })
      }
      # return()
      tbl <- self$AsTable()
      # cat("Adding Roche quantities\n")
      for (target in dilutions.r %>>% names()) {
        for (r.id in dilutions.r[[target]] %>>% names()) {
          sample.name <- filter(tbl, react.id == r.id)$sample[1]
          private$.sample[[sample.name]]$quantity <- 
            quantityType$new(
              value = unname(dilutions.r[[1]][r.id]),
              unit = quantityUnitType$new("other")
            )
          private$.sample[[sample.name]]$annotation <- 
            c(private$.sample[[sample.name]]$annotation,
              annotationType$new(
                property = sprintf("Roche_quantity_at_%s_%s",
                                   target,
                                   r.id),
                value = as.character(dilutions.r[[target]][r.id])))
        }
      }
      
      # cat("Adding Roche conditions\n")
      for (r.id in conditions.r %>>% names()) {
        sample.name <- filter(tbl, react.id == r.id)$sample[1]
        private$.sample[[sample.name]]$annotation <- 
          c(private$.sample[[sample.name]]$annotation,
            annotationType$new(
              property = sprintf("Roche_condition_at_%s",r.id),
              value = conditions.r[r.id]))
      }
    }
    
  }
  
  # file format select -------------------------------------------------
  if (format == "auto")
    format <- file_ext(filename)
  switch (
    format,
    eds = {
      fromABI()
      return()
    },
    xlsx = {
      fromExcel()
      return()
    },
    csv = {
      fromCSV()
      return()
    },
    rex = {
      fromRotorGene()
      return()
    },
    {
      fromRDML()
      return()
    }
  )
}, 
overwrite = TRUE)
