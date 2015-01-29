#' Sets fluorescence data vectors to \code{RDML} object
#' 
#' Write docs here
#' 
#' @examples
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
#' cpp.gg <- cfx96$GetFData(tab, data.type = "cpp", long.table = TRUE)
#' plot1 <- ggplot(cfx96.gg, aes(x = cyc, y = fluo,group=fdata.name)) + geom_line() + ggtitle("Raw data")
#' plot2 <- ggplot(cpp.gg, aes(x = cyc, y = fluo,group=fdata.name)) + geom_line() + ggtitle("CPP processed data")
#' grid.arrange(plot1, plot2, nrow=2)
#' @docType methods
#' @name SetFData
#' @aliases RDML.SetFData
#' @rdname setfdata-method
#' @include RDML.R
RDML$set("public", "SetFData",
         function(data,
                  description) {
           data.type <- substitute(data)
           fdata.names <- colnames(data)
           first.col.name <- fdata.names[1]
           for(i in 2:ncol(data)) {
             descr.i <- which(description$fdata.name == fdata.names[i])
             descr.row <- unlist(description[descr.i, ])
             private$.experiment[[descr.row["exp.id"]]]$
               run[[descr.row["run.id"]]]$
               react[[descr.row["react.id"]]]$
               data[[descr.row["target"]]][[deparse(data.type)]] <-
               matrix(c(data[, 1], data[, i]), 
                      byrow = FALSE,
                      ncol = 2,
                      dimnames = list(NULL,
                                      c(first.col.name, "fluor")))
           }
         }
         , overwrite = TRUE)