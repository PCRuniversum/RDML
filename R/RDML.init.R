rdml.env <- new.env(parent = emptyenv())

# XML parsing helpers -------------------------------------------------
getTextValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml_find_first(tree, path, ns)
  if (class(node) == "xml_missing")
         return(NULL)
  xml_text(node)
}

getTextVector <- function(tree, path, ns = rdml.env$ns) {
  xml_text(xml_find_all(tree, path, ns))
}

getLogicalValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml_find_first(tree, path, ns)
  if (class(node) == "xml_missing")
    return(NULL)
  as.logical(xml_text(node))
}

getNumericValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml_find_first(tree, path, ns)
  if (class(node) == "xml_missing")
    return(NULL)
  as.numeric(xml_text(node))
}

# getNumericVector <- function(tree, path, ns = rdml.env$ns) {
#   node.set <- xml_find_all(tree, path, ns)
#   if (length(node.set) == 0)
#     return(NULL)
#   list.mapv(node.set,
#             as.numeric(xml_text(.)))
# }

getNumericVector <- function(tree, path, ns = rdml.env$ns) {
  as.numeric(
    xml_text(xml_find_all(tree, path, ns)))
}

getIntegerValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml_find_first(tree, path, ns)
  if (class(node) == "xml_missing")
    return(NULL)
  xml_integer(node)
}

getIntegerVector <- function(tree, path, ns = rdml.env$ns) {
  xml_integer(xml_find_all(tree, path, ns))
}

genId <- function(node) {
  idType$new(xml_attr(node, "id"))
}

genIdRef <- function(node) {
  idReferencesType$new(xml_attr(node, "id"))
}

as.numeric <- function(val) {
  out <- tryCatch(
    base::as.numeric(val),
    warning = function(w) {
      base::as.numeric(gsub(",", ".", val))
    }
  )
  if (length(out))
    return(out)
  NULL
}

as.integer <- function(val) {
  out <- base::as.integer(val)
  if (length(out))
    return(out)
  NULL
}

# Misc functions -------------------------------------------------
ns <- NULL

FromPositionToId <- function(react.id, 
                             pcrFormat = pcrFormatType$new(8, 12, 
                                                           labelFormatType$new("ABC"), 
                                                           labelFormatType$new("123"))) {
  row <- which(LETTERS ==
                 gsub("([A-Z])[0-9]+", "\\1", react.id))
  col <- as.integer(gsub("[A-Z]([0-9]+)", "\\1", react.id))
  # as.character((row - 1) * 12 + col)
  (row - 1) * pcrFormat$columns + col
}

GetIds <- function(l) {
  unname(sapply(l, function(el) el$id))
}

# Gets concentrations (quantity) of each
# dilution from XML for Roche
GetDilutionsRoche <- function(uniq.folder)
{
  # cat("\nParsing Roche standards data...")
  if (!file.exists(paste0(uniq.folder,"/calculated_data.xml"))) {
    # cat("NO SUCH FILE")
    return(NA)
  }
  rdml.doc <- read_xml(paste0(uniq.folder,"/calculated_data.xml"))
  if (length(xml_ns(rdml.doc)) != 9) {
    return(NULL)
  }
  rdml.env$ns <- xml_ns_rename(xml_ns(rdml.doc), d1 = "calc", d2 = "analys", d3 = "quant")
  # xml_ns_strip(rdml.doc)
  concs <- getNumericVector(rdml.doc, "//quant:absQuantDataSource/quant:standard")
  if (length(concs) == 0) {
    concs <- getNumericVector(rdml.doc, "//quant:relQuantDataSource/quant:standard")
    concs.guids <-
      getTextVector(rdml.doc, "//quant:relQuantDataSource/standard/../quant:graphId")
  } else {
    concs.guids <-
      getTextVector(rdml.doc, "//quant:absQuantDataSource/quant:standard/../quant:graphId")
  }
  names(concs) <- concs.guids
  concs <- sort(concs, decreasing = TRUE)
  positions <-
    getTextVector(rdml.doc,
                  "//quant:standardPoints/quant:standardPoint/quant:position")
  positions <- sapply(positions, FromPositionToId)
  dye.names <- getTextVector(rdml.doc,
                             "//quant:standardPoints/quant:standardPoint/quant:dyeName")
  positions.guids <-
    getTextVector(rdml.doc,
                  "//quant:standardPoints/quant:standardPoint/quant:graphIds/quant:guid")
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
  rdml.env$ns <- xml_ns_rename(xml_ns(rdml.doc), d1 = "lc96")
  nodes <- xml_find_all(rdml.doc,
                        "/lc96:rocheLC96AppExtension/lc96:experiment/lc96:run/lc96:react/lc96:condition/..",
                        ns = rdml.env$ns)
  reacts <- xml_attr(nodes, "id")
  conditions <- getTextVector(nodes, "lc96:condition")
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
  rdml.env$ns <- xml_ns_rename(xml_ns(rdml.doc), d3 = "rel")
  ref <- xml_find_all(rdml.doc,
                      "//rel:geneSettings/rel:relQuantGeneSettings",
                      ns = rdml.env$ns)
  if (length(ref) == 0) {
    # cat("NONE")
    return(NULL)
  }
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
#'   <stefan.roediger@@b-tu.de>, Michal Burdukiewicz
#'   <michalburdukiewicz@@gmail.com>
#' @docType methods
#' @name new
#' @aliases RDML.new
#' @rdname new-method
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom lubridate ymd_hms ymd
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
                                          cluster = NULL,
                                          format = "auto") {
  if(missing(filename)) {
    return()
  }
  assertString(filename)
  
  # ABI7500 -----------------------------------------------------------------
  fromABI <- function() {
    # tryCatch({
    uniq.folder <- tempfile()
    unzipped <- unzip(filename, exdir = uniq.folder)
    # multicomponent.data <- paste0(uniq.folder, "\\apldbio\\sds\\multicomponent_data.txt") %>>%
    #   read.delim(skip = 2, stringsAsFactors = FALSE) %>>%
    #   filter(!is.na(WELL))
    data.file <- paste0(uniq.folder, "/apldbio/sds/multicomponent_data.txt")
    multicomponent.data <- readChar(data.file, file.info(data.file)$size) %>>%
      str_match_all("([0-9]+)\\t([0-9]+)\\t([A-Z]+)\\t(?:Infinity)?(?:NaN)?[0-9E\\-]*\\.?[0-9E\\-]*\\t([0-9E\\-]+\\.?[0-9E\\-]*)")
    multicomponent.data <- as.data.frame(multicomponent.data[[1]], stringsAsFactors = FALSE)
    names(multicomponent.data) <- c("_", "well", "cyc", "dye", "fluor")
    multicomponent.data <- multicomponent.data %>>%
      data.table()
    multicomponent.data[, c("well", "cyc", "fluor") := list(as.numeric(well),
                                                            as.numeric(cyc),
                                                            as.numeric(fluor))]
    ncycles <- multicomponent.data$cyc %>>% max + 1
    
    plate.setup <- paste0(uniq.folder, "/apldbio/sds/plate_setup.xml") %>>%
      read_xml()
    
    rdml.env$ns <- xml_ns(plate.setup)
    snames <- getTextVector(plate.setup,
                            "/Plate/FeatureMap/Feature[Id='sample']/../FeatureValue/FeatureItem/Sample/Name")
    names(snames) <-
      getIntegerVector(plate.setup,
                       "/Plate/FeatureMap/Feature[Id='sample']/../FeatureValue/Index") + 1
    
    description <- data.frame()
    list.iter(xml_find_all(plate.setup,
                           "/Plate/FeatureMap/Feature[Id='detector-task']/../FeatureValue"),
              el ~ {
                index <- getIntegerValue(el, "Index") + 1
                
                #only one task allowed!
                task <- getTextValue(el, "FeatureItem/DetectorTaskList/*[1]/Task") %>>%
                  switch (
                    UNKNOWN = "unkn",
                    NTC = "ntc",
                    STANDARD = "std"
                  )
                # size <- .[["FeatureItem"]][["DetectorTaskList"]] %>>% xmlSize
                list.iter(xml_find_all(el, "FeatureItem/DetectorTaskList"),
                          sub.el ~ {
                            description <<-
                              rbind(description,
                                    data.frame(fdata.name =
                                                 paste(index,
                                                       getTextVector(sub.el,
                                                                     "DetectorTask/Detector/Reporter")),
                                               exp.id = "exp1",
                                               run.id = "run1",
                                               react.id = index,
                                               sample = ifelse(is.na(snames[as.character(index)]),
                                                               "unnamed",
                                                               snames[as.character(index)]) %>>% unname(),
                                               sample.type = task,
                                               target = getTextVector(sub.el, "DetectorTask/Detector/Name"),
                                               target.dyeId = getTextVector(sub.el, "DetectorTask/Detector/Reporter"),
                                               quantity = getNumericVector(sub.el, "DetectorTask/Concentration"),
                                               IsOmit = FALSE,
                                               stringsAsFactors = FALSE))
                          })})
    
    description <- data.table(description)
    omitted.i <- getIntegerVector(plate.setup,
                                  "/Plate/Wells/Well[IsOmit='true']/Index") + 1
    description[react.id %in% omitted.i, IsOmit := TRUE]
    description <- description[IsOmit == FALSE]
    fdata <- cbind(data.frame(multicomponent.data$cyc %>>% unique() + 1),
                   apply(description, 1,
                         function(r) {
                           multicomponent.data[well == as.integer(r["react.id"]) - 1 &
                                                 dye == r["target.dyeId"],
                                               fluor]
                         }))
    names(fdata) <- c("cyc", description$fdata.name)
    self$SetFData(fdata, description)
    self$id <- list(rdmlIdType$new("ABI" , "1"))
    # },
    # error = function(e) { stop(e) },
    # finally = 
    unlink(uniq.folder, recursive = TRUE)
  }
  
  # RotorGene -----------------------------------------------------------------
  fromRotorGene <- function() {
    dat <- filename %>>%
      read_xml()
    description <- dat %>>%
      xml_find_all("/Experiment/Samples/Page/Sample/Name/text()[1]/../..") %>>%
      list.map(el ~ {
        el <- list.flatten(as_list(el))
        list(
          fdata.name = el$ID,
          exp.id = "exp1",
          run.id = "run1",
          react.id = el$TubePosition %>>% as.numeric(),
          sample = el$Name,
          sample.type = switch(el$Type,
                               "5" = "pos",
                               "3" = "ntc",
                               "1" = "std",
                               "unkn"),
          quantity = el$GivenConc %>>% as.numeric()
        )}) %>>%
      list.stack()
    # %>>%
    #   data.table() %>>%
    #   set(,)
    #   mutate(react.id = react.id %>>% as.numeric,
    #          quantity = quantity %>>% as.numeric)
    dat %>>%
      xml_find_all("/Experiment/Samples/Groups/Group") %>>%
      list.iter(group ~ {
        group <- as_list(group)
        ids <- group[-c(1, 2)] %>>% list.mapv(Tube)
        description[description$fdata.name %in% ids, "target"] <<- group$Name
      })
    rdml.env$ns <- xml_ns(dat)
    
    original.targets <- description$target
    
    dat %>>%
      xml_find_all("/Experiment/RawChannels/RawChannel") %>>%
      list.iter(rawChannel ~ {
        # rawChannel <- as_list(rawChannel)
        description$target.dyeId <<- getTextValue(rawChannel, "Name")
        description$target <<- paste(original.targets,
                                     description$target.dyeId[1], sep = "#")
        fdata <-
          getTextVector(rawChannel,
                        sprintf(
                          "Name[text()='%s']/../Reading",
                          description$target.dyeId[1]
                        ))[description$react.id] %>>%
          list.map(x ~ {strsplit(x, " ")[[1]] %>>%
              as.numeric() %>>%
              as.list()}) %>>%
          list.stack() %>>%
          t()
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
  
  # From DTprime dna-technology ------------------------------------------------------
  fromDTprime <- function() {
    DT96_OVERLOAD_SIGNAL <- 15000 # ??
    lns <- readLines(con <- file(filename)) %>>%
      str_replace_all("\\t", " ")
    close(con)
    # lns <- stri_encode(lns, "", "UTF-8")
    tubes.info <- lns[(which(grepl("\\$Information about tubes:\\$", lns)) + 1):
                        (which(lns == "$MultiChannel:$ ") - 2)] %>>%
      str_split(" ")
    concentrations <- lns[(which(lns == "$MultiChannel:$ ") + 1):
                            (which(grepl("\\$Device", lns)) - 3)] %>>%
      str_split(" ")
    kits <- lns[(which(lns == "$Information about TESTs:$ ") + 1):
                  (which(lns == "$Parameters MutationMC:$ ") - 1)] %>>%
      str_split(" ")
    
    fdata.raw <- lns[(which(lns == "$Results of optical measurements:$ ") + 1):
                       length(lns)] %>>%
      str_split("\\s+") %>>%
      transpose() %>>%
      setDT() %>>%
      setnames(c("dye", "x1", "x2", "x3", "cycle", "exposition", "background",
                 paste("tube", 0:95, sep = "_"),
                 "?"))
    
    fdata.raw.nrow <- nrow(fdata.raw)
    expositions <- c(fdata.raw[1, exposition],
                     fdata.raw[2, exposition])
    fdata <- data.table(cyc = 1:(fdata.raw.nrow / 10))
    description <- data.table()
    list.iter(tubes.info,
              tube ~ {
                tube.name <- tube[10]
                if (tube.name != "-") {
                  list.iter(c("FAM", "HEX", "ROX", "Cy5", "Cy5.5"),
                            dye ~ {
                              tube.id <- sprintf("tube_%s", tube[2])
                              if (fdata.raw[(.i - 1) * 2 + 1, get(tube.id)] != "1") {
                                dye.i <- .i
                                fdata.dye.2000 <-
                                  fdata.raw[seq((dye.i - 1) * 2 + 1,
                                                fdata.raw.nrow, by = 10),
                                            as.numeric(get(tube.id))] -
                                  fdata.raw[seq((dye.i - 1) * 2 + 1,
                                                fdata.raw.nrow, by = 10),
                                            as.numeric(background)]
                                fdata.dye.400 <-
                                  fdata.raw[seq((dye.i - 1) * 2 + 2,
                                                fdata.raw.nrow, by = 10),
                                            as.numeric(get(tube.id))] -
                                  fdata.raw[seq((dye.i - 1) * 2 + 2,
                                                fdata.raw.nrow, by = 10),
                                            as.numeric(background)]
                                fdata.dye.comb <- {
                                  if (all(fdata.dye.2000 < DT96_OVERLOAD_SIGNAL)) {
                                    fdata.dye.2000
                                  } else {
                                    fdata.dye.400 * 5
                                  }
                                }
                                
                                set(fdata, ,
                                    sprintf("%s_%s_%s", tube.name, dye,
                                            "2000"),
                                    fdata.dye.2000)
                                set(fdata, ,
                                    sprintf("%s_%s_%s", tube.name, dye,
                                            "400"),
                                    fdata.dye.400)
                                set(fdata, ,
                                    sprintf("%s_%s_%s", tube.name, dye,
                                            "comb"),
                                    fdata.dye.comb)
                                descr.2000 <- list(
                                  fdata.name = sprintf("%s_%s_%s", tube.name, dye,
                                                       "2000"),
                                  exp.id = ".2000",
                                  run.id = "run1",
                                  react.id = as.integer(tube[2]) + 1,
                                  sample = tube.name,
                                  target = sprintf("%s#%s",
                                                   list.filter(kits,
                                                               kit ~ kit[2] == tube[9])[[1]][3],
                                                   dye),
                                  target.dyeId = dye,
                                  sample.type = "unkn",
                                  # only FAM quantity
                                  quantity = tryCatch(list.filter(concentrations,
                                                                  conc ~ conc[2] == tube[2])[[1]][4] %>>%
                                                        as.numeric(),
                                                      error = function(e) NA)
                                )
                                descr.400 <- descr.2000 %>>%
                                  (dt ~ {
                                    dt$fdata.name <- sprintf("%s_%s_%s", tube.name, dye,
                                                             "400")
                                    dt$exp.id <- ".400"
                                    dt
                                  })
                                descr.comb <- descr.2000 %>>%
                                  (dt ~ {
                                    dt$fdata.name <- sprintf("%s_%s_%s", tube.name, dye,
                                                             "comb")
                                    dt$exp.id <- "combined"
                                    dt
                                  })
                                description <<-
                                  rbind(description,
                                        descr.2000)
                                description <<-
                                  rbind(description,
                                        descr.400)
                                description <<-
                                  rbind(description,
                                        descr.comb)
                              }
                            })
                }
              })
    # self$SetFData(fdata, stri_encode(description, "", "UTF-8"))
    self$SetFData(fdata, description)
    self$id <- list(rdmlIdType$new("DTprime" , "1"))
  }
  
  # From CSV -----------------------------------------------------------------
  fromCSV <- function() {
    pcrdata <- read.csv(filename)
    fdata.names <- colnames(pcrdata)[-1]
    data.type <- {
      if (tolower(colnames(pcrdata)[1]) == "tmp" ||
          tolower(colnames(pcrdata)[1]) == "temperature")
        "mdp"
      else
        "adp"
    }
    descr <- data.frame(
      fdata.name  = fdata.names,
      exp.id = "exp1",
      run.id = "run1",
      react.id = 1:length(fdata.names),
      sample = fdata.names,
      target = "unkn",
      target.dyeId = "unkn",
      sample.type = "unkn",
      stringsAsFactors = FALSE
    )
    self$SetFData(pcrdata, descr, fdata.type = data.type)
  }
  
  # COBAS -----------------------------------------------------------------
  # fromCobas <- function() {
  #   dat <- filename %>>%
  #     read_xml()
  #   
  #   nms <- dat %>>%
  #     xml_find_all("/Experiment/Subsets/Subset[prop[@name='SubsetID']= '0']/list/item")
  #   
  #   samples <- trimws(xml_text(nms))
  #   numsamples <- length(samples)
  #   
  #   for (j in 1:2)
  #   {
  #     trg <- switch(j, "FAM", "HEX")
  #     description <- data.frame(
  #       fdata.name  = 1:numsamples,
  #       exp.id = "exp1",
  #       run.id = "run1",
  #       react.id = 1:numsamples,
  #       sample = samples,
  #       sample.type = "unkn",
  #       quantity = 0,
  #       target = sprintf("unkn@unkn#%s", trg),
  #       target.dyeId = trg,
  #       stringsAsFactors = FALSE
  #     )
  #     rdml.env$ns <- xml_ns(dat)
  #     
  #     fdata <- NULL
  #     for (i in 1:numsamples)
  #     {
  #       nms <- dat %>>%
  #         xml_find_all(sprintf("/Experiment/run/Acquisitions/Sample[@Number='%d']/Acq/Chan[@Number='%d']/prop[@name='Fluor']",
  #                              i, j - 1))
  #       
  #       vals <- trimws(xml_text(nms))
  #       fdata <- cbind(fdata,as.numeric(c(vals)))
  #     }
  #     
  #     colnames(fdata) <- description$react.id
  #     fdata <- cbind(cyc = 1:nrow(fdata), fdata)
  #     #fdata <- as.data.frame.matrix(fdata)
  #     #print("fdata")
  #     #print(fdata[1])
  #     #print(fdata[2,])
  #     #print(typeof(fdata))
  #     self$SetFData(fdata, description)
  #   }
  #   self$id <- list(rdmlIdType$new("Roche Diagnostics" , "1"))
  #   #print (self)
  # }#fromCobas
  
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
    
    # tryCatch({
    # Roche use more than one file at RDML zip.
    # One of the files store dilutions information.
    if (length(unzipped.rdml) > 1) {
      # cat("\nParsing Roche(?) data...")
      rdml.doc <- read_xml(paste0(uniq.folder,"/rdml_data.xml"))
      dilutions.r <- GetDilutionsRoche(uniq.folder)
      conditions.r <- GetConditionsRoche(uniq.folder)
      ref.genes.r <- GetRefGenesRoche(uniq.folder)
      rdml.env$ns <- xml_ns_rename(xml_ns(rdml.doc), "d1" = "rdml")
    } else {
      # cat("\nParsing data...")
      rdml.doc <- read_xml(unzipped.rdml)
      rdml.env$ns <- xml_ns(rdml.doc)
      if (!("rdml" %in% names(rdml.env$ns))) {
        rdml.env$ns <- xml_ns_rename(xml_ns(rdml.doc), d1 = "rdml")
      }
    }
    # },
    # error = function(e) { stop(e) },
    # finally = unlink(uniq.folder, recursive = TRUE)
    # )
    unlink(uniq.folder, recursive = TRUE)
    
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
                   publisher = getTextValue(tree = id, path = "rdml:publisher"),
                   serialNumber = getTextValue(tree = id, path = "rdml:serialNumber"),
                   MD5Hash = getTextValue(tree = id, path = "rdml:MD5Hash")
                 )
      ) %>>%
      list.names(.$publisher)
    # cat("\nGetting experementer")
    private$.experimenter <- {
      # experimenter.list <-
      list.map(rdml.doc %>>%
                 xml_find_all("/rdml:rdml/rdml:experimenter", rdml.env$ns),
               experimenter ~
                 experimenterType$new(
                   id = genId(experimenter),
                   firstName = getTextValue(experimenter, "rdml:firstName"),
                   lastName = getTextValue(experimenter, "rdml:lastName"),
                   email = getTextValue(experimenter, "rdml:email"),
                   labName = getTextValue(experimenter, "rdml:labName"),
                   labAddress = getTextValue(experimenter, "rdml:labAddress")
                 )) %>>%
        list.names(.$id$id)
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
                   text = getTextValue(documentation, "rdml:text")
                 )) %>>%
        list.names(.$id$id)
    }
    
    # dye -----------------------------------------------------------------
    # cat("\nGetting dye")
    private$.dye <- {
      list.map(rdml.doc %>>%
                 xml_find_all("/rdml:rdml/rdml:dye", rdml.env$ns),
               dye ~ dyeType$new(
                 id = genId(dye),
                 description = getTextValue(dye, "rdml:description")
               )) %>>%
        list.names(.$id$id)
    }
    
    # sample -----------------------------------------------------------------
    # cat("\nGetting sample")
    private$.sample <-
      list.map(rdml.doc %>>%
                 xml_find_all("/rdml:rdml/rdml:sample", rdml.env$ns),
               sample ~
               {
                 type <- getTextValue(sample, "rdml:type")
                 
                 # remove Roche omitted ('ntp') samples
                 if(type == "ntp")
                   return(NULL)
                 id <- xml_attr(sample, "id")
                 sampleType$new(
                   id = idType$new(id),
                   description = getTextValue(sample, "rdml:description"),
                   documentation =
                     lapply(sample %>>%
                              xml_find_all("rdml:documentation", rdml.env$ns),
                            function(doc) genIdRef(doc)),
                   xRef =
                     list.map(sample %>>%
                                xml_find_all("rdml:xRef", rdml.env$ns),
                              xRef ~ xRefType$new(
                                name = getTextValue(xRef, "rdml:name"),
                                id = getTextValue(xRef, "rdml:id")
                              )),
                   annotation = c(
                     list.map(sample %>>%
                                xml_find_all("rdml:annotation", rdml.env$ns),
                              annotation ~ annotationType$new(
                                property = getTextValue(annotation, "rdml:property"),
                                value = getTextValue(annotation, "rdml:value")
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
                     getLogicalValue(sample, "rdml:interRunCalibrator"),
                   quantity =
                     tryCatch(
                       quantityType$new(
                         value = getNumericValue(sample, "rdml:quantity/rdml:value"),
                         unit = quantityUnitType$new(
                           getTextValue(sample, "rdml:quantity/rdml:unit"))),
                       error = function(e) NULL
                     ),
                   calibratorSample =
                     getLogicalValue(sample, "rdml:calibaratorSample"),
                   cdnaSynthesisMethod = cdnaSynthesisMethodType$new(
                     enzyme = getTextValue(sample, "rdml:cdnaSynthesisMethod/rdml:enzyme"),
                     primingMethod =
                       primingMethodType$new(getTextValue(sample,
                                                          "rdml:cdnaSynthesisMethod/rdml:primingMethod")),
                     dnaseTreatment = getLogicalValue(sample, "rdml:cdnaSynthesisMethod/rdml:dnaseTreatment"),
                     thermalCyclingConditions =
                       tryCatch(
                         genIdRef(xml_find_first(sample,
                                                 "rdml:cdnaSynthesisMethod/rdml:thermalCyclingConditions",
                                                 ns = rdml.env$ns)),
                         error = function(e) NULL)
                   ),
                   templateQuantity =
                     tryCatch(
                       templateQuantityType$new(
                         conc = getNumericValue(sample, "rdml:templateQuantity/rdml:conc"),
                         nucleotide = nucleotideType$new(
                           getTextValue(sample, "rdml:templateQuantity/rdml:nucleotide"))
                       ),
                       error = function(e) NULL
                     )
                 )
               }) %>>%
      list.filter(!is.null(.)) %>>%
      list.names(.$id$id)
    
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
                   description = getTextValue(target, "rdml:description"),
                   documentation =
                     lapply(target %>>%
                              xml_find_all("rdml:documentation", rdml.env$ns),
                            function(doc) genIdRef(doc)),
                   xRef =
                     list.map(target %>>%
                                xml_find_all("rdml:xRef", rdml.env$ns),
                              xRef ~
                                xRefType$new(
                                  name = getTextValue(xRef, "rdml:name"),
                                  id = getTextValue(xRef, "rdml:id")
                                )),
                   type = targetTypeType$new(getTextValue(target, "rdml:type")),
                   amplificationEfficiencyMethod =
                     getTextValue(target, "rdml:amplificationEfficiencyMethod"),
                   amplificationEfficiency =
                     getNumericValue(target, "rdml:amplificationEfficiency"),
                   amplificationEfficiencySE =
                     getNumericValue(target, "rdml:amplificationEfficiencySE"),
                   detectionLimit =
                     getNumericValue(target, "rdml:detectionLimit"),
                   dyeId =
                     tryCatch(
                       target %>>%
                         xml_find_first("rdml:dyeId", rdml.env$ns) %>>%
                         genIdRef(),
                       # StepOne stores dyeId as xml value
                       error = function(e)
                         idReferencesType$new(getTextValue(target, "rdml:dyeId"))
                     ),
                   # dyeId = NA,
                   
                   sequences = sequencesType$new(
                     forwardPrimer =
                       tryCatch(
                         oligoType$new(
                           threePrimeTag =
                             getTextValue(target, "rdml:sequences/rdml:forwardPrimer/rdml:threePrimeTag"),
                           fivePrimeTag =
                             getTextValue(target, "rdml:sequences/rdml:forwardPrimer/rdml:fivePrimeTag"),
                           sequence =
                             getTextValue(target, "rdml:sequences/rdml:forwardPrimer/rdml:sequence")),
                         error = function(e) NULL
                       ),
                     reversePrimer =
                       tryCatch(
                         oligoType$new(
                           threePrimeTag =
                             getTextValue(target, "rdml:sequences/rdml:reversePrimer/rdml:threePrimeTag"),
                           fivePrimeTag =
                             getTextValue(target, "rdml:sequences/rdml:reversePrimer/rdml:fivePrimeTag"),
                           sequence =
                             getTextValue(target, "rdml:sequences/rdml:reversePrimer/rdml:sequence")),
                         error = function(e) NULL
                       ),
                     probe1 =
                       tryCatch(
                         oligoType$new(
                           threePrimeTag =
                             getTextValue(target, "rdml:sequences/rdml:probe1/rdml:threePrimeTag"),
                           fivePrimeTag =
                             getTextValue(target, "rdml:sequences/rdml:probe1/rdml:fivePrimeTag"),
                           sequence =
                             getTextValue(target, "rdml:sequences/rdml:probe1/rdml:sequence")),
                         error = function(e) NULL
                       ),
                     probe2 =
                       tryCatch(
                         oligoType$new(
                           threePrimeTag =
                             getTextValue(target, "rdml:sequences/rdml:probe2/rdml:threePrimeTag"),
                           fivePrimeTag =
                             getTextValue(target, "rdml:sequences/rdml:probe2/rdml:fivePrimeTag"),
                           sequence =
                             getTextValue(target, "rdml:sequences/rdml:probe2/rdml:sequence")),
                         error = function(e) NULL
                       ),
                     amplicon =
                       tryCatch(
                         oligoType$new(
                           threePrimeTag =
                             getTextValue(target, "rdml:sequences/rdml:amplicon/rdml:threePrimeTag"),
                           fivePrimeTag =
                             getTextValue(target, "rdml:sequences/rdml:amplicon/rdml:fivePrimeTag"),
                           sequence =
                             getTextValue(target, "rdml:sequences/rdml:amplicon/rdml:sequence")),
                         error = function(e) NULL
                       )),
                   commercialAssay =
                     tryCatch(
                       commercialAssayType$new(
                         company =
                           getTextValue(target, "rdml:commercialAssay/rdml:company"),
                         orderNumber =
                           getTextValue(target, "rdml:commercialAssay/rdml:orderNumber"))
                       ,
                       error = function(e) NULL
                     )
                 )
               }
      ) %>>%
      list.names(.$id$id)
    
    # thermalCyclingConditions -------------------------------------------------
    # cat("\nGetting thermalCyclingConditions")
    private$.thermalCyclingConditions <-
      list.map(rdml.doc %>>%
                 xml_find_all("/rdml:rdml/rdml:thermalCyclingConditions", rdml.env$ns),
               tcc ~ {
                 thermalCyclingConditionsType$new(
                   id = genId(tcc),
                   description = getTextValue(tcc, "rdml:description"),
                   documentation =
                     lapply(tcc %>>%
                              xml_find_all("rdml:documentation", rdml.env$ns),
                            function(doc) genIdRef(doc)),
                   lidTemperature =
                     getNumericValue(tcc, "rdml:lidTemperature"),
                   
                   experimenter =
                     list.map(tcc %>>%
                                xml_find_all("rdml:experimenter", rdml.env$ns),
                              experimenter ~ genIdRef(experimenter)
                     ),
                   
                   step = list.map(tcc %>>%
                                     xml_find_all("rdml:step", rdml.env$ns),
                                   step ~ {
                                     stepType$new(
                                       nr = getIntegerValue(step, "rdml:nr"),
                                       description = getTextValue(step, "rdml:description"),
                                       temperature = {
                                         tryCatch(
                                           temperatureType$new(
                                             temperature =
                                               getNumericValue(step, "rdml:temperature/rdml:temperature"),
                                             duration =
                                               getIntegerValue(step, "rdml:temperature/rdml:duration"),
                                             temperatureChange =
                                               getNumericValue(step, "rdml:temperature/rdml:temperatureChange"),
                                             durationChange =
                                               getIntegerValue(step, "rdml:temperature/rdml:durationChange"),
                                             measure = measureType$new(
                                               getTextValue(step, "rdml:temperature/rdml:measure")),
                                             ramp =
                                               getNumericValue(step, "rdml:temperature/rdml:ramp")
                                           ),
                                           error = function(e) NULL)},
                                       gradient = {
                                         tryCatch(
                                           gradientType$new(
                                             highTemperature =
                                               getNumericValue(step, "rdml:gradient/rdml:highTemperature"),
                                             lowTemperature =
                                               getNumericValue(step, "rdml:gradient/rdml:lowTemperature"),
                                             duration =
                                               getIntegerValue(step, "rdml:gradient/rdml:duration"),
                                             temperatureChange =
                                               getNumericValue(step, "rdml:gradient/rdml:temperatureChange"),
                                             durationChange =
                                               getIntegerValue(step, "rdml:gradient/rdml:durationChange"),
                                             measure = measureType$new(
                                               getTextValue(step, "rdml:gradient/rdml:measure")),
                                             ramp =
                                               getNumericValue(step, "rdml:gradient/rdml:ramp")),
                                           error = function(e) NULL)
                                       },
                                       loop = {
                                         tryCatch(
                                           loopType$new(
                                             goto = getIntegerValue(step, "rdml:loop/rdml:goto"),
                                             # should be called "repeat" but this is reserved word
                                             repeat.n = getIntegerValue(step, "rdml:loop/rdml:repeat")),
                                           error = function(e) NULL)},
                                       pause = {
                                         tryCatch(
                                           pauseType$new(
                                             temperature =
                                               getNumericValue(step, "rdml:pause/rdml:temperature")),
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
      list.names(.$id$id)
    #     names(tcc.list) <- GetIds(tcc.list)
    #     tcc.list
    # data -------------------------------------------------
    GetData <- function(data) {
      tar.id <-
        data %>>%
        xml_find_first("rdml:tar", rdml.env$ns) %>>%
        xml_attr("id")
      dataC <- as.character(data)
      dataType$new(
        tar = idReferencesType$new(
          if (length(unzipped.rdml) > 1 &&
              length(private$.id) != 0 &&
              private$.id[[1]]$publisher == "Roche Diagnostics")
            gsub("@(.+)$", "\\1",
                 regmatches(tar.id, gregexpr("@(.+)$", tar.id))[[1]])
          else
          {
            if (is.na(tar.id))
              "NA"
            else
              tar.id
          }),
        cq = getNumericValue(data, "rdml:cq"),
        excl = getTextValue(data, "rdml:excl"),
        adp = {
            fpoints <- str_match_all(dataC,
                                     "<adp>\\\\?n?\\s*<cyc>(.*)</cyc>\\\\?n?\\s*<tmp>(.*)</tmp>\\\\?n?\\s*<fluor>(.*)</fluor>\\\\?n?\\s*</adp>")[[1]][,-1]
            
            if (length(fpoints)) {
              # check for duplicate cycles. Occures in StepOne RDML files.
              # if (base::anyDuplicated(fpoints$cyc)) {
              #   fpoints <- fpoints[-base::duplicated(fpoints$cyc)]
              #   warning("Duplicate cycles removed")
              # }
              adpsType$new(
                # data.table(cyc = cyc, tmp = tmp, fluor = fluor)
                data.table(cyc = as.numeric(fpoints[, 1]), 
                           tmp = as.numeric(fpoints[, 2]), 
                           fluor = as.numeric(fpoints[, 3]))
              )
            } else {
              fpoints <- str_match_all(dataC,
                                       "<adp>\\\\?n?\\s*<cyc>(.*)</cyc>\\\\?n?\\s*<fluor>(.*)</fluor>\\\\?n?\\s*</adp>")[[1]][,-1]
              if (length(fpoints)) {
                adpsType$new(
                  data.table(cyc = as.numeric(fpoints[, 1]),
                             fluor = as.numeric(fpoints[, 2]))
                )
              } else {
                NULL
              }
            }
        },
        mdp = {
          fpoints <- str_match_all(dataC,
                                   "<mdp>\\\\?n?\\s*<tmp>(.*)</tmp>\\\\?n?\\s*<fluor>(.*)</fluor>\\\\?n?\\s*</mdp>")[[1]][,-1]
          if (length(fpoints)) {
            mdpsType$new(
              data.table(tmp = as.numeric(fpoints[, 1]),
                         fluor = as.numeric(fpoints[, 2]))
            )
          } else {
            NULL
          }
        },
        endPt = getNumericValue(data, "rdml:endPt"),
        bgFluor = getNumericValue(data, "rdml:bgFluor"),
        bgFluorSlp = getNumericValue(data, "rdml:bgFluorSp"),
        quantFluor = getNumericValue(data, "rdml:quantFluor")
      )
    }

    # react -------------------------------------------------
    GetReact <- function(react,
                         pcrFormat = pcrFormatType$new(
                           8, 12, 
                           labelFormatType$new("ABC"), 
                           labelFormatType$new("123"))) {
      react.id <- xml_attr(react, "id")
      react.id.corrected <- tryCatch(
        as.integer(react.id),
        warning = function(w) {
          # if react.id is 'B1' not '13'
          # like in StepOne
          FromPositionToId(react.id, pcrFormat)
        }
      )
      #     cat(sprintf("\nreact: %i", react.id))
      sample <-
        react %>>%
        xml_find_first("rdml:sample", rdml.env$ns) %>>%
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
                     xml_find_all("rdml:data", rdml.env$ns),
                   data ~ GetData(data)
          )
        }
      )
    }
    
    # run -------------------------------------------------
    GetRun <- function(run, experiment.id) {
      run.id <- xml_attr(run, "id")
      pcrFormat <- {
        pcrFormatStr <- getTextValue(run, "rdml:pcrFormat")
        # Quantstudio pcrFormat
        if (!is.null(pcrFormatStr) && grepl("well", pcrFormatStr)) {
          if (grepl("96-well", pcrFormatStr)) {
            pcrFormatType$new(
              rows = 8,
              columns = 12,
              rowLabel = labelFormatType$new("ABC"),
              columnLabel = labelFormatType$new("123")
            )
          } else {
            pcrFormatType$new(
              rows = 16,
              columns = 24,
              rowLabel = labelFormatType$new("ABC"),
              columnLabel = labelFormatType$new("123")
            )
          }
        } else {# correct RDML pcrFormat
          rows <- getIntegerValue(run, "rdml:pcrFormat/rdml:rows")
          # check for absent of 'pcrFormat' like in StepOne
          if (!is.null(rows) && !is.na(rows)) {
            pcrFormatType$new(
              rows = rows,
              columns = getIntegerValue(run, "rdml:pcrFormat/rdml:columns"),
              rowLabel = labelFormatType$new(
                getTextValue(run, "rdml:pcrFormat/rdml:rowLabel")),
              columnLabel = labelFormatType$new(
                getTextValue(run, "rdml:pcrFormat/rdml:columnLabel"))
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
      }
      if (show.progress)
        cat(sprintf("\n\trun: %s\n", run.id))
      runType$new(
        id = idType$new(run.id),
        description = getTextValue(run, "rdml:description"),
        documentation =
          lapply(run %>>%
                   xml_find_all("rdml:documentation", rdml.env$ns),
                 function(doc) genIdRef(doc)),
        experimenter =
          list.map(run %>>%
                     xml_find_all("rdml:experimenter", rdml.env$ns),
                   experimenter ~ genIdRef(experimenter)
          ),
        instrument = getTextValue(run, "rdml:instrument"),
        dataCollectionSoftware =
          tryCatch(
            dataCollectionSoftwareType$new(
              name = getTextValue(run, "rdml:dataCollectionSoftware/rdml:name"),
              version = getTextValue(run, "rdml:dataCollectionSoftware/rdml:version")
            ),
            error = function(e) NULL),
        backgroundDeterminationMethod =
          getTextValue(run, "rdml:backgroundDeterminationMethod"),
        cqDetectionMethod =
          cqDetectionMethodType$new(getTextValue(run, "rdml:cqDetectionMethod")),
        thermalCyclingConditions =
          tryCatch(
            run %>>%
              xml_find_first("rdml:thermalCyclingConditions", rdml.env$ns) %>>%
              genIdRef(),
            error = function(e) NULL),
        pcrFormat = pcrFormat,
        runDate = getTextValue(run, "rdml:runDate"),
        react = 
          list.map(run %>>%
                     xml_find_all("rdml:react", rdml.env$ns),
                   react ~
                     GetReact(react,
                              pcrFormat)
          ) %>>%
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
        description = getTextValue(experiment, "rdml:description"),
        documentation =
          lapply(experiment %>>%
                   xml_find_all("rdml:documentation", rdml.env$ns),
                 function(doc) genIdRef(doc)),
        run =
          list.map(experiment %>>%
                     xml_find_all("rdml:run", rdml.env$ns),
                   run ~ GetRun(run)
          )
      )
    }
    
    
    private$.experiment <-
      list.map(rdml.doc %>>%
                 xml_find_all("rdml:experiment", rdml.env$ns),
               experiment ~ GetExperiment(experiment)
      ) %>>%
      list.names(.$id$id)
    
    # Combine CFX96 runs to one (by default Bio-Rad use separate run for each dye!---
    if (!is.null(private$.id) &&
        length(private$.id) != 0 &&
        !is.null(private$.id[[1]]$publisher) &&
        private$.id[[1]]$publisher == "Bio-Rad Laboratories, Inc." &&
        length(private$.experiment[[1]]$run) > 1) {
      if (show.progress)
        cat("\nCombining Bio-Rad runs\n")
      first.run <- private$.experiment[[1]]$run[[1]]
      for (run.i in 2:length(private$.experiment[[1]]$run)){
        current.run <- private$.experiment[[1]]$run[[run.i]]
        for (react in current.run$react){
          react.id <- as.character(react$id$id)
          if (is.null(first.run$react[[react.id]])) {
            first.run$react[[react.id]] <- react
          } else {
            first.run$react[[react.id]]$data <- c(
              first.run$react[[react.id]]$data,
              react$data[[1]]
            )
          }
        }
      }
      # delete copied runs
      for (run.i in length(private$.experiment[[1]]$run):2){
        private$.experiment[[1]]$run[[run.i]] <- NULL
      }
      private$.experiment[[1]]$run[[1]]$id <- idType$new("Combined Run")
    }
    
    # Roche LC96 extra parsing -------------------------------------------------
    # parse original!!! Roche files
    if (length(unzipped.rdml) > 1 &&
        length(private$.id) != 0 &&
        private$.id[[1]]$publisher == "Roche Diagnostics") {
      for (i in 1:length(private$.sample)) {
        private$.sample[[i]]$id <- idType$new(private$.sample[[i]]$description)
      }
      private$.sample <- list.names(private$.sample,
                                    .$id$id)
      
      # cat("Adding Roche ref genes\n")
      if (!is.null(ref.genes.r) &&
          !is.na(ref.genes.r) &&
          length(ref.genes.r) != 0) {
        ns <- xml_ns_rename(xml_ns(ref.genes.r), d3 = "rel")
        list.iter(ref.genes.r,
                  ref.gene ~ {
                    geneName <- getTextValue(ref.gene, "rel:geneName",
                                             ns = ns)
                    geneI <- grep(
                      sprintf("^%s$", geneName),
                      names(private$.target))
                    private$.target[[geneI]]$type <-
                      targetTypeType$new(
                        ifelse(getLogicalValue(ref.gene, "rel:isReference",
                                               ns = ns),
                               "ref",
                               "toi"))
                  })
      }
      # return()
      tbl <- self$AsTable() %>>%
        setkey(react.id)
      # cat("Adding Roche quantities\n")
      for (target in dilutions.r %>>% names()) {
        for (r.id in dilutions.r[[target]] %>>% names()) {
          sample.name <- tbl[react.id == as.integer(r.id), sample][1]
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
        sample.name <- tbl[as.integer(r.id), sample,
                           mult = "first"]
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
    r96 = {
      fromDTprime()
      return()
    },
    # xml = {
    #   fromCobas()
    #   return()
    # },
    {
      fromRDML()
      return()
    }
  )
},
overwrite = TRUE)
