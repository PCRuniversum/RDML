#' Clones \code{RDML} objects
#' 
#' Clones \code{RDML} object for properly work with RDML object copy.
#' 
#' @docType methods
#' @name Clone
#' @aliases RDML.Clone
#' @rdname clone-method
#' @include RDML.R
#' @include RDML.Merge.R
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
RDML$set("public", "Clone",
         function() {
           clone <- RDML$new()
           clone$Merge(self)
           return(clone)
         },
         overwrite = TRUE)