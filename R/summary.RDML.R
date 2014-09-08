summary.RDML_object <- function(object, print = TRUE, ...) {
  dil_table <- do.call(rbind, object[["Dilutions"]])
  rownames(dil_table) <- names(object[["Dilutions"]])
  
  exp_tab <- data.frame(do.call(rbind, lapply(object[["qPCR"]], function(i) {
    tab <- do.call(rbind, lapply(i, function(j)
      summary(MFIaggr(j), print = FALSE)))
    data.frame(type = rownames(tab), tab, row.names = NULL)
    })), row.names = NULL)
  exp_tab <- cbind(experiment = as.vector(sapply(names(object[["qPCR"]]), function(i) 
    rep(i, 2))), exp_tab) 
  cat("Dilutions:\n")
  print(dil_table)
  
  #first three moments
  first_three <- exp_tab[, c("experiment", "type", "mean", "sd", "skewness")]
  colnames(first_three) <- c("Experiment", "Type", "Mean", "Standard deviation", "Skewness")
  cat("\nSummarized experiments - first three moments:\n")
  print(first_three)
  
  #resistant statistics
  resist_stats <- exp_tab[, c("experiment", "type", "median", "mad", "IQR", "medcouple")]
  colnames(resist_stats) <- c("Experiment", "Type", "Median", "Median Absolute Deviation", 
                             "Interquartile Range", "Medcouple")
  cat("\nSummarized experiments - resistant statistics:\n")
  print(resist_stats)
  
  #linear
  lin_stats <- exp_tab[, c("experiment", "type", "intercept", "slope", "r.squared", 
                           "heter.p")]
  colnames(lin_stats) <- c("Experiment", "Type", "Intercept", "Slope", 
                           "R squared", "Breusch-Pagan Test p-value")
  cat("\nSummarized experiments - linear model statistics:\n")
  print(lin_stats)
  
  #rest
  rest_stats <- exp_tab[, c("experiment", "type", "SNR", "VRM", "NAs")]
  colnames(rest_stats) <- c("Experiment", "Type", "SNR", "VRM", "Number of NAs")
  cat("\nSummarized experiments - other statistics:\n")
  print(rest_stats)
  
  cat("\n")
  
  invisible(list(dil_table = dil_table, exp_tab = exp_tab))
}