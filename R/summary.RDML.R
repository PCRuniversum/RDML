summary.RDML_object <- function(object, print = TRUE, ...) {
  dilTable <- do.call(rbind, object[["Dilutions"]])
  rownames(dilTable) <- names(object[["Dilutions"]])
  
  expTab <- data.frame(do.call(rbind, lapply(object[["qPCR"]], function(i) {
    tab <- do.call(rbind, lapply(i, function(j)
      summary(MFIaggr(j), print = FALSE)))
    data.frame(type = rownames(tab), tab, row.names = NULL)
    })), row.names = NULL)
  expTab <- cbind(experiment = as.vector(sapply(names(object[["qPCR"]]), function(i) 
    rep(i, 2))), expTab) 
  cat("Dilutions:\n")
  print(dilTable)
  
  #first three moments
  firstThree <- expTab[, c("experiment", "type", "mean", "sd", "skewness")]
  colnames(firstThree) <- c("Experiment", "Type", "Mean", "Standard deviation", "Skewness")
  cat("\nSummarized experiments - first three moments:\n")
  print(firstThree)
  
  #resistant statistics
  resistStats <- expTab[, c("experiment", "type", "median", "mad", "IQR", "medcouple")]
  colnames(resistStats) <- c("Experiment", "Type", "Median", "Median Absolute Deviation", 
                             "Interquartile Range", "Medcouple")
  cat("\nSummarized experiments - resistant statistics:\n")
  print(resistStats)
  
  #linear
  linStats <- expTab[, c("experiment", "type", "intercept", "slope", "r.squared", 
                           "heter.p")]
  colnames(linStats) <- c("Experiment", "Type", "Intercept", "Slope", 
                           "R squared", "Breusch-Pagan Test p-value")
  cat("\nSummarized experiments - linear model statistics:\n")
  print(linStats)
  
  #rest
  restStats <- expTab[, c("experiment", "type", "SNR", "VRM", "NAs")]
  colnames(restStats) <- c("Experiment", "Type", "SNR", "VRM", "Number of NAs")
  cat("\nSummarized experiments - other statistics:\n")
  print(restStats)
  
  cat("\n")
  
  invisible(list(dilTable = dilTable, expTab = expTab))
}