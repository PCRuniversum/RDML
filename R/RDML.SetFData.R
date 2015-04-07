#' Sets fluorescence data vectors to \code{RDML} object
#' 
#' Sets fluorescence data vectors to \code{RDML} object for specified method
#' of experiment.
#' 
#' @param data \code{matrix} that contains at first column constant data for all fluorescence vectors(i.e. cycle numbers or temperature)
#' and fluorescence values at other columns. Name of column is the name of constant data.
#' Names of other column are \code{fdata.names} (links to rows at \code{description}). Name of
#' matrix passed to param becomes type of data.
#' @param description output from \code{AsTable} function that descrips fluorescence data.
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
#' cfx96.qPCR <- cfx96$GetFData(tab)
#' cpp <- cbind(cyc = cfx96.qPCR[, 1],
#'  apply(cfx96.qPCR[, -1], 2, 
#'    function(y) CPP(x = cfx96.qPCR[, 1], y = y)$y.norm))
#' cfx96$SetFData(cpp, tab)
#' library(ggplot2)
#' library(gridExtra)
#' cfx96.gg <- cfx96$GetFData(tab, long.table = TRUE)
#' cpp.gg <- cfx96$GetFData(tab, fdata.type = "cpp",
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
#' @name SetFData
#' @aliases RDML.SetFData
#' @rdname setfdata-method
#' @include RDML.R
RDML$set("public", "SetFData",
         function(data,
                  description) {
           fdata.type <- substitute(data)
           fdata.names <- colnames(data)
           first.col.name <- fdata.names[1]
           for(i in 2:ncol(data)) {
             descr.i <- which(description$fdata.name == fdata.names[i])
             descr.row <- unlist(description[descr.i, ])
             private$.experiment[[descr.row["exp.id"]]]$
               run[[descr.row["run.id"]]]$
               react[[descr.row["react.id"]]]$
               data[[descr.row["target"]]][[deparse(fdata.type)]] <-
               matrix(c(data[, 1], data[, i]), 
                      byrow = FALSE,
                      ncol = 2,
                      dimnames = list(NULL,
                                      c(first.col.name, "fluor")))
           }
         }
         , overwrite = TRUE)