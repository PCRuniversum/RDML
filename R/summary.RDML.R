summary.RDML_object <- function(object, print = TRUE, ...) {
  if("Dilutions" %in% names(object)) {
    if(typeof(object[["Dilutions"]]) == "list") {
      dilTable <- do.call(rbind, object[["Dilutions"]])
      rownames(dilTable) <- names(object[["Dilutions"]])
    }
    else {
      dilTable <- object[["Dilutions"]]
    }
    if(print) {
      cat("Dilutions:\n")
      print(dilTable)
      cat("\n")
    }
  }
  if("Melt" %in% names(object)) {
    if(attr(object, "flat.table")) {
      meltList <- suppressMessages(apply(object[["Melt"]][, -1], 2, function(i)
        diffQ2(cbind(object[["Melt"]][, 1], i))))
      meltTable <- t(sapply(meltList, function(i) c(i[["Tm"]], i[["fluoTm"]],
                                                    i[["xTm1.2.D2"]], i[["yTm1.2.D2"]])))
      colnames(meltTable) <- c("Tm1D1", "FluoTm1", "Tm1D2", "Tm2D2", "FluoTm1D2",
                               "FluoTm2D2")
    } else {
      meltList <- suppressMessages(lapply(object[["Melt"]], function(dye) 
        lapply(dye, function(experiment)
          apply(experiment[, -1], 2, function(i)
            diffQ2(cbind(experiment[, 1], i))))))
      
      meltNames <- do.call(rbind, unlist(lapply(1L:length(meltList), function(dye) 
        lapply(1L:length(meltList[[dye]]), function(type)
          t(sapply(names(meltList[[dye]][[type]]), function(i)
            c(names(meltList)[dye], i))))), FALSE))
      
      meltTable <- matrix(unlist(lapply(meltList, function(dye) 
        lapply(dye, function(type)
          lapply(type, function(experiment)
            c(experiment[["Tm"]], experiment[["fluoTm"]],
              experiment[["xTm1.2.D2"]], experiment[["yTm1.2.D2"]]))))), 
        ncol = 6, byrow = TRUE)
      meltTable <- cbind(data.frame(meltNames), data.frame(meltTable))
      
      colnames(meltTable) <- c("Dye", "Experiment", "Tm1D1", "FluoTm1", "Tm1D2", 
                               "Tm2D2", "FluoTm1D2", "FluoTm2D2")
    }
    
    if(print) {
      cat("\nTable of melting temperatures:\n")
      print(meltTable) 
      cat("\n")
    }
  }
  
  if("qPCR" %in% names(object)) {
    if (attr(object, "flat.table")) {
      expTable <- summary(MFIaggr(object[["qPCR"]]), print = print)
    } else {
      #iterate MFI aggr over all experiments and all types of experiments
      expTable <- data.frame(do.call(rbind, lapply(object[["qPCR"]], function(i) {
        tab <- do.call(rbind, lapply(i, function(j)
          summary(MFIaggr(j), print = FALSE)))
        data.frame(type = rownames(tab), tab, row.names = NULL)
      })), row.names = NULL)
      
      #generate names of experiments and replicate them by number of types
      expTable <- cbind(experiment = as.vector(sapply(names(object[["qPCR"]]), function(i) 
        rep(i, length(object[["qPCR"]][[1]])))), expTable) 
      
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
  
  if(exists("meltTable"))
    res <- c(res, meltTable = list(meltTable))
  
  if(exists("expTable"))
    res <- c(res, expTable = list(expTable))
  
  invisible(res)
}