#' S3 wrapper for \code{RDML$Summarize()} method
#' 
#' See \link{RDML.Summarize}.
#' 
#' 
#' @keywords manip summary
#' @export
#' @include RDML.R
summary.RDML <- function(object, print = TRUE, ...) {
  object$Summarize(print, ...)
}

#' Summary RDML R6 objects
#' 
#' Prints and silently returns summary statistics of \code{RDML} objects.
#' 
#' @name Summarize
#' @aliases RDML.Summarize
#' @rdname summarize-method
#' @docType methods
#' @param print logical, if \code{TRUE} summary is printed to console.
#' @param ...  currently ignored.
#' @return A list of length at most 3 (less if \code{object} does not contain
#' information about dilutions, qPCR or melting data): \item{dilTable}{A list
#' of dilutions used in experiments.} \item{meltTable}{Melting data - currently
#' not implemented.} \item{expTable}{Summary statistics describing experiments:
#' mean, median, standard deviation, median absolute deviation, interquartile
#' range, medcouple, skewness, SNR, VRM, number of NAs (missing values),
#' intercept, slope, R squared, Breusch-Pagan test p-value.}
#' @author Michal Burdukiewicz, Konstantin A. Blagodatskikh, Stefan Roediger
#' @seealso Summary statistics are calculated using
#' \code{\link[chipPCR]{MFIaggr}}.
#' @keywords manip summary
#' @examples
#' 
#' lc96 <- RDML$new(system.file("extdata/lc96_bACTXY.rdml", package = "RDML"))
#' summary(lc96)
#' #store raw results of summary without printing
#' res <- lc96$Summarize(print = FALSE)
RDML$set("public", "Summarize", function(print = TRUE, ...) {
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
  
  runStr <- do.call(rbind, unlist(lapply(self$targets, function(target)
    lapply(self$types[[target]], function(stype)
      data.frame(Targets = target, 
                 Types = stype, 
                 No.Samples =  length(private$.plate.map[[1]])))), 
    recursive = FALSE))
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
, overwrite = TRUE)