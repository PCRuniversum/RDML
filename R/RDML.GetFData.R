#' Gets RDML
#' 
#' Gets RDML
#' 
#' @name GetFData
#' @include RDML.R
RDML$set("public", "GetFData", function(filter = list(method = "qPCR")) {
  params.names <- names(filter)
  if(!("method" %in% params.names))
    filter$method <- "qPCR"
  if("react.ids" %in% params.names)
    filtered.indices <- which(
      private$.plate.map$ReactID %in% as.character(
        filter$react.ids))
  else if("tubes" %in% params.names)
    filtered.indices <- which(
      private$.plate.map$Tube %in% filter$tubes)
  else { 
    filtered.map <- private$.plate.map
    if("targets" %in% params.names)
      filtered.map <- filtered.map[which(
        filtered.map$Target %in% filter$targets),]
    if("types" %in% params.names)
      filtered.map <-filtered.map[
        which(filtered.map$Type %in% filter$types),]
    if("names" %in% params.names)
      filtered.map <-filtered.map[
        grep(filter$names, 
             filtered.map$FDataName),]
  }
  filtered.fdata.ids <- filtered.map$FDataID
  if(!is.null(filter$method) && filter$method == "melt") {
    filtered <- as.data.frame(private$.melt.fdata[,filtered.fdata.ids])
    filtered <- cbind(as.numeric(rownames(private$.melt.fdata)),
                      filtered)
    names(filtered) <- c("Temp.", 
                         filtered.map$FDataName)
  }
  else {
    filtered <- as.data.frame(
      private$.qPCR.fdata[,filtered.fdata.ids],
      row.names = rownames(private$.qPCR.fdata))
    filtered <- cbind(as.numeric(rownames(private$.qPCR.fdata)),
                      filtered)
    names(filtered) <- c("Cycles", 
                         filtered.map$FDataName)
  }
  return(filtered)
}
, overwrite = TRUE)