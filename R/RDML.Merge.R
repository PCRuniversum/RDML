#' Merges \code{RDML} objects
#' 
#' Merges one \code{RDML} object into another.
#' 
#' @param to.merge \code{RDML} object that should be merged into current object.
#' 
#' @docType methods
#' @name Merge
#' @aliases RDML.Merge
#' @rdname merge-method
#' @include RDML.R
#' @export
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
MergeRDMLs <- function(to.merge) {
  baseRDML <- to.merge[[1]]$Clone()
  for(rdml in to.merge[-1]) {
    for(element in c("id",
                     "experimenter",
                     "documentation",
                     "dye",
                     "sample",
                     "target",
                     "thermalCyclingConditions",
                     "experiment"
    )) {
      if (length(rdml[[element]]) != 0)
        baseRDML[[element]] <- c(baseRDML[[element]],
                                 llply(rdml[[element]],
                                  function(subelement) subelement$Clone()))
    }
  }
  baseRDML
}