selectFData <- function(object,
                        melt = FALSE,
                        targets = NA, 
                        types = NA, 
                        snames = NA) 
  {
  if (class(object) != "RDML_object") stop("object must be of class 'RDML_object'!")
  runtype <- ifelse(melt,
                    "Melt",
                    "qPCR")
  # Check for flat.table = TRUE
  if (typeof(object[[runtype]][[1]]) != "list")
  {      
      output <- object[[runtype]][1]
      if(is.na(snames)) {
          output <- cbind(output, object[[runtype]][-1])
      }
      else {
          cols <- grep(snames, names(object[[runtype]]))
          cols <- unique(cols)    
          output <- cbind(output, object[[runtype]][cols])
      }
      if (length(output) == 1) return(data.frame())
      return(output)
  }

  #### executes when flat.table=FALSE
  # if targets=NA, then all available targets in run
  if (anyNA(targets)) targets <- names(object[[runtype]])
  # if types=NA, then all available types for specified targets
  if (anyNA(types)) {
    types <- as.vector(sapply(targets, function(fluorTarget) {
      names(object[[runtype]][[fluorTarget]])      
    }))    
    types <- unique(types)    
  }  
  # add first column with cycles
  output <- object[[runtype]][[targets[1]]][[types[1]]][1]
  for(fluorTarget in targets)
  {
    for(fluorType in types)
    {      
      if(is.na(snames)) {
        output <- cbind(output, object[[runtype]][[fluorTarget]][[fluorType]][-1])
      }
      else {
        cols <- grep(snames, names(object[[runtype]][[fluorTarget]][[fluorType]]))
        cols <- unique(cols)
        output <- cbind(output, object[[runtype]][[fluorTarget]][[fluorType]][cols])
      }
    }
  }
  if (length(output) == 1) return(data.frame())
  return(output)
}