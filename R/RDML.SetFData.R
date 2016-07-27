#' Sets fluorescence data vectors to \code{RDML} object
#' 
#' Sets fluorescence data vectors to \code{RDML} object for specified method
#' of experiment.
#' 
#' @param data \code{matrix} containing in the first column data corresponding to 
#' all fluorescence values in the following columns. The name of the first column 
#' is the name of variable and names of other column are \code{fdata.names} (links 
#' to rows at \code{description}).
#' @param description output from \code{AsTable} function that describes fluorescence data.
#' @param fdata.type 'adp' for qPCR, 'mdp' for melting data.
#' 
#' @examples
#' \dontrun{
#' PATH <- path.package("RDML")
#' filename <- paste0(PATH, "/extdata/", "stepone_std.rdml")
#' cfx96 <- RDML$new(filename)
#' ## Use plotCurves function from the chipPCR package to 
#' ## get an overview of the amplification curves
#' library(chipPCR)
#' ## Extract all qPCR data 
#' tab <- cfx96$AsTable()
#' tab2 <- tab
#' tab2$run.id <- "cpp"
#' cfx96.qPCR <- cfx96$GetFData(tab)
#' cpp <- cbind(cyc = cfx96.qPCR[, 1],
#'  apply(cfx96.qPCR[, -1], 2, 
#'    function(y) CPP(x = cfx96.qPCR[, 1], y = y)$y.norm))
#' cfx96$SetFData(cpp, tab2)
#' library(ggplot2)
#' library(gridExtra)
#' cfx96.gg <- cfx96$GetFData(tab, long.table = TRUE)
#' cpp.gg <- cfx96$GetFData(tab2,
#'                          long.table = TRUE)
#' plot1 <- ggplot(cfx96.gg, aes(x = cyc, y = fluo,
#'                 group=fdata.name)) +
#'                  geom_line() +
#'                  ggtitle("Raw data")
#' plot2 <- ggplot(cpp.gg, aes(x = cyc, y = fluo,
#'                 group=fdata.name)) +
#'                  geom_line() +
#'                  ggtitle("CPP processed data")
#' grid.arrange(plot1, plot2, nrow=2)
#' }
#' @docType methods
#' @name RDML.SetFData
#' @rdname setfdata-method
#' @include RDML.R
RDML$set("public", "SetFData",
         function(fdata,
                  description,
                  fdata.type = "adp") {
           #            first.col.name <- ifelse(fdata.type == "adp",
           #                                     "cyc",
           #                                     "tmp")
           fdata <- data.table(fdata)
           names(fdata)[1] <- "cyc"
           
           description <- data.table(description)
           
           fdata.names <- colnames(fdata)[2:ncol(fdata)]
           
           for (fdata.n in fdata.names) {
             descr.row <- description[fdata.name == fdata.n]
             if (nrow(descr.row) == 0) {
               warning(paste("No such decription:", fdata.n))
               next
             }
             if (nrow(descr.row) > 1) {
               stop(paste("Multiply decription:", fdata.n))
             }
             # adds unavailable subelements
             if (private$.experiment[[descr.row[, exp.id]]]$
                 run[[descr.row[, run.id]]]$
                 react[[as.character(descr.row[, react.id])]]$
                 data[[descr.row[, target]]] %>>% is.null()) {
               if (private$.experiment[[descr.row[, exp.id]]] %>>% 
                   is.null()) {
                 self$experiment <- 
                   c(private$.experiment,
                     experimentType$new(idType$new(descr.row[, exp.id])))
               }
               if (private$.experiment[[descr.row[, exp.id]]]$
                   run[[descr.row[, run.id]]] %>>% 
                   is.null()) {
                 private$.experiment[[descr.row[, exp.id]]]$run <- 
                   c(private$.experiment[[descr.row[, exp.id]]]$run,
                     runType$new(
                       idType$new(descr.row[, run.id]),
                       pcrFormat = 
                         pcrFormatType$new(8, 12,
                                           labelFormatType$new("ABC"),
                                           labelFormatType$new("123"))))
               }
               if (private$.experiment[[descr.row[, exp.id]]]$
                   run[[descr.row[, run.id]]]$
                   react[[as.character(descr.row[, react.id])]] %>>% 
                   is.null()) {
                 private$.experiment[[descr.row[, exp.id]]]$
                   run[[descr.row[, run.id]]]$react <- 
                   c(private$.experiment[[descr.row[, exp.id]]]$
                       run[[descr.row[, run.id]]]$react,
                     reactType$new(
                       reactIdType$new(descr.row[, react.id]),
                       sample = idReferencesType$new(descr.row[, sample])))
                 if (private$.sample[[descr.row[, sample]]] %>>% 
                     is.null()) {
                   self$sample <- c(
                     self$sample,
                     sampleType$new(idType$new(descr.row[, sample]),
                                    type = sampleTypeType$new(descr.row[, sample.type]))
                     
                   )
                 }
               }
               
               if (private$.experiment[[descr.row[, exp.id]]]$
                   run[[descr.row[, run.id]]]$
                   react[[as.character(descr.row[, react.id])]]$
                   data[[descr.row[, target]]] %>>% 
                   is.null()) {
                 private$.experiment[[descr.row[, exp.id]]]$
                   run[[descr.row[, run.id]]]$
                   react[[as.character(descr.row[, react.id])]]$
                   data <- 
                   c(private$.experiment[[descr.row[, exp.id]]]$
                       run[[descr.row[, run.id]]]$
                       react[[as.character(descr.row[, react.id])]]$
                       data,
                     dataType$new(idReferencesType$new(
                       descr.row[, target])
                     ))
                 if (private$.target[[descr.row[, target]]] %>>% 
                     is.null()) {
                   self$target <- c(
                     self$target,
                     targetType$new(idType$new(descr.row[, target]),
                                    type = targetTypeType$new("toi"),
                                    dyeId = idReferencesType$new(
                                      descr.row[, target.dyeId]
                                    ))
                   )
                 }
                 if (private$.dye[[descr.row[, target.dyeId]]] %>>% 
                     is.null()) {
                   self$dye <- c(
                     self$dye,
                     dyeType$new(idType$new(descr.row[, target.dyeId]))
                   )
                 }
               }
             }
             private$.experiment[[descr.row[, exp.id]]]$
               run[[descr.row[, run.id]]]$
               react[[as.character(descr.row[, react.id])]]$
               data[[descr.row[, target]]][[fdata.type]] <- {
                 if (fdata.type == "adp") {
                   adpsType$new(
                     data.table(cyc = fdata[, cyc], fluor = fdata[, get(fdata.n)]))
                 } else {
                   mdpsType$new(
                     data.table(tmp = fdata[, cyc], fluor = fdata[, get(fdata.n)]))
                 }
               }
           }
           
           
           
           # add concentrations 
           if (!is.null(description$quantity)) {
             for (sample.name in description$sample %>>% unique()) {
               if (!is.na(description[sample == sample.name, quantity][1])) {
                 private$.sample[[sample.name]]$quantity <- 
                   quantityType$new(
                     value = unname(description[sample == sample.name, quantity][1]),
                     unit = quantityUnitType$new("other")
                   )
               }
             }
           }
           
         }
         , overwrite = TRUE)