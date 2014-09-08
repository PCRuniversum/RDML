summary.RDML_object <- function(object, print = TRUE, ...) {
  if("Dilutions" %in% names(object)) {
    dil_table <- do.call(rbind, object[["Dilutions"]])
    rownames(dil_table) <- names(object[["Dilutions"]])
    
    if(print) {
      cat("Dilutions:\n")
      print(dil_table)
    }
  }
  
  if("Melt" %in% names(object)) {
    #FIX ME!
    melt_table <- c()
  }
  
  if("qPCR" %in% names(object)) {
    exp_table <- data.frame(do.call(rbind, lapply(object[["qPCR"]], function(i) {
      tab <- do.call(rbind, lapply(i, function(j)
        summary(MFIaggr(j), print = FALSE)))
      data.frame(type = rownames(tab), tab, row.names = NULL)
    })), row.names = NULL)
    exp_table <- cbind(experiment = as.vector(sapply(names(object[["qPCR"]]), function(i) 
      rep(i, 2))), exp_table) 
    
    if(print) {
      #first three moments
      first_three <- exp_table[, c("experiment", "type", "mean", "sd", "skewness")]
      colnames(first_three) <- c("Experiment", "Type", "Mean", "Standard deviation", "Skewness")
      cat("\nSummarized experiments - first three moments:\n")
      print(first_three)
      
      #resistant statistics
      resist_stats <- exp_table[, c("experiment", "type", "median", "mad", "IQR", "medcouple")]
      colnames(resist_stats) <- c("Experiment", "Type", "Median", "Median Absolute Deviation", 
                                  "Interquartile Range", "Medcouple")
      cat("\nSummarized experiments - resistant statistics:\n")
      print(resist_stats)
      
      #linear
      lin_stats <- exp_table[, c("experiment", "type", "intercept", "slope", "r.squared", 
                                 "heter.p")]
      colnames(lin_stats) <- c("Experiment", "Type", "Intercept", "Slope", 
                               "R squared", "Breusch-Pagan Test p-value")
      cat("\nSummarized experiments - linear model statistics:\n")
      print(lin_stats)
      
      #rest
      rest_stats <- exp_table[, c("experiment", "type", "SNR", "VRM", "NAs")]
      colnames(rest_stats) <- c("Experiment", "Type", "SNR", "VRM", "Number of NAs")
      cat("\nSummarized experiments - other statistics:\n")
      print(rest_stats)
      
      cat("\n")
    }
  }
  res <- list()
  
  if(exists("dil_table"))
    res <- c(res, dil_table = list(dil_table))
  
  if(exists("melt_table"))
    res <- c(res, melt_table = list(melt_table))
  
  if(exists("exp_table"))
    res <- c(res, exp_table = list(exp_table))
  
  invisible(res)
}