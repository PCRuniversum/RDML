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
  
  # if targets=NA, then all available targets in run
  if (anyNA(targets)) targets <- names(object[[runtype]])
  
  # add first column with cycles ot temperatures
  output <- object[[runtype]][[1]][[1]][1]
  
  for(fluorTarget in targets)
  {
    at.target.types <- names(object[[runtype]][[fluorTarget]])
    at.target.types <- unique(at.target.types)
    # if types=NA, then all available types for current target
    if (is.na(types)) use.types <-  at.target.types
    else use.types <- intersect(at.target.types, types)    
    for(fluorType in use.types)
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