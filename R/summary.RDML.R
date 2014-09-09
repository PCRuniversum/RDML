summary.RDML_object <- function(object, print = TRUE, ...) {
  if("Dilutions" %in% names(object)) {
    dilTable <- do.call(rbind, object[["Dilutions"]])
    rownames(dilTable) <- names(object[["Dilutions"]])
    
    if(print) {
      cat("Dilutions:\n")
      print(dilTable)
    }
  }
  
  if("Melt" %in% names(object)) {
    #FIX ME!
    #HERE PLACE for diffQ2 from MBmca package
    meltTable <- c()
  }
  
  if("qPCR" %in% names(object)) {
    if (class(object[["qPCR"]]) == "data.frame") {
      expTable <- summary(MFIaggr(object[["qPCR"]]), print = print)
    } else {
      expTable <- data.frame(do.call(rbind, lapply(object[["qPCR"]], function(i) {
        tab <- do.call(rbind, lapply(i, function(j)
          summary(MFIaggr(j), print = FALSE)))
        data.frame(type = rownames(tab), tab, row.names = NULL)
      })), row.names = NULL)
      expTable <- cbind(experiment = as.vector(sapply(names(object[["qPCR"]]), function(i) 
        rep(i, 2))), expTable) 
      
      if(print) {
        #first three moments
        firstThree <- expTable[, c("experiment", "type", "mean", "sd", "skewness")]
        colnames(firstThree) <- c("Experiment", "Type", "Mean", 
                                  "Standard deviation", "Skewness")
        cat("\nSummarized experiments - first three moments:\n")
        print(firstThree)
        
        #resistant statistics
        resistStats <- expTable[, c("experiment", "type", "median", "mad", 
                                    "IQR", "medcouple")]
        colnames(resistStats) <- c("Experiment", "Type", "Median", 
                                   "Median Absolute Deviation", 
                                   "Interquartile Range", "Medcouple")
        cat("\nSummarized experiments - resistant statistics:\n")
        print(resistStats)
        
        #linear
        linStats <- expTable[, c("experiment", "type", "intercept", "slope", 
                                 "r.squared", "heter.p")]
        colnames(linStats) <- c("Experiment", "Type", "Intercept", "Slope", 
                                "R squared", "Breusch-Pagan Test p-value")
        cat("\nSummarized experiments - linear model statistics:\n")
        print(linStats)
        
        #rest
        restStats <- expTable[, c("experiment", "type", "SNR", "VRM", "NAs")]
        colnames(restStats) <- c("Experiment", "Type", "SNR", "VRM", 
                                 "Number of NAs")
        cat("\nSummarized experiments - other statistics:\n")
        print(restStats)
        
        cat("\n")
      }
    }
  }
  res <- list()
  
  if(exists("dilTable"))
    res <- c(res, dilTable = list(dilTable))
  
#   if(exists("meltTable"))
#     res <- c(res, meltTable = list(meltTable))
  
  if(exists("expTable"))
    res <- c(res, expTable = list(expTable))
  
  invisible(res)
}