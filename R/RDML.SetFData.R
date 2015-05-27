#' Sets fluorescence data vectors to \code{RDML} object
#' 
#' Sets fluorescence data vectors to \code{RDML} object for specified method
#' of experiment.
#' 
#' @param data \code{matrix} that contains in the first column constant data for all fluorescence vectors (i.e. cycle numbers or temperature)
#' and fluorescence values in the following columns. The column name is the name of constant data.
#' Names of other column are \code{fdata.names} (links to rows at \code{description}). Name of
#' matrix passed to param becomes type of data.
#' @param description output from \code{AsTable} function that describes fluorescence data.
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
         function(fdata,
                  fdata.type = "adp",
                  description) {
           first.col.name <- ifelse(fdata.type == "adp",
                                    "cyc",
                                    "tmp")
           fdata.names <- colnames(fdata)[2:ncol(fdata)]
           for(fdata.n in fdata.names) {
             descr.row <- description %>% 
               filter(fdata.name == fdata.n) %>% 
               unlist
             if (private$.experiment[[descr.row["exp.id"]]]$
                 run[[descr.row["run.id"]]]$
                 react[[descr.row["react.id"]]]$
                 data[[descr.row["target"]]] %>% is.null)
               stop(sprintf("Wrong path: %s",
                            paste(descr.row, collapse = ", ")))
             private$.experiment[[descr.row["exp.id"]]]$
               run[[descr.row["run.id"]]]$
               react[[descr.row["react.id"]]]$
               data[[descr.row["target"]]][[fdata.type]] <-
               matrix(c(fdata[, 1], fdata[, fdata.n]), 
                      byrow = FALSE,
                      ncol = 2,
                      dimnames = list(NULL,
                                      c(first.col.name, "fluor")))
           }
         }
         , overwrite = TRUE)