layerToXML  <- function(parent, layer) {
  for(name in names(layer)) {
    if (typeof(layer[[name]]) == "list") {
      sublayer <- 
        newXMLNode(name, 
                   attrs = attributes(
                     layer[[name]])[names(attributes(layer[[name]])) != "names"],
                   parent = parent)
      layerToXML(sublayer, layer[[name]])
    } else if(!is.na(layer[[name]])) {
      newXMLNode(name,
                 layer[[name]],
                 attrs = attributes(
                   layer[[name]])[names(attributes(layer[[name]])) != "names"],
                 parent = parent)
    }
  }
}

#' Write \code{RDML} objects
#' 
#' Writes \code{RDML} object.
#' 
#' @docType methods
#' @name Write
#' @aliases RDML.Write
#' @rdname write-method
#' @include RDML.R
#' @include RDML.Write.R
#' @examples
#' \dontrun{
#' PATH <- path.package("RDML")
#' filename <- paste(PATH, "/extdata/", "lc96_bACTXY.rdml", sep ="")
#' lc96 <- RDML$new(filename)
#' filename <- paste(PATH, "/extdata/", "stepone_std.rdml", sep ="")
#' stepone <- RDML$new(filename)
#' lc96$Merge(stepone)
#' lc96$AsTable()
#' }
RDML$set("public", "Write",
         function() {
           rdmlroot <- newXMLNode("RDML")
           layerToXML(rdmlroot, private)
           return(rdmlroot)
         },
         overwrite = TRUE)