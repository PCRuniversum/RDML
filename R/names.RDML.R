#' The Names of the RDML object
#' 
#' Just wraps \code{ls()} function to show RDML public fields
#' and methods. Useful for autocomplete at R editors.
#' 
#' @aliases names.RDML names,RDML-method
#' @param object an object of class \code{RDML}.
#' @param ... other arguments passed to \code{\link[base]{ls}} function.
#' @export
#' @include RDML.R
names.RDML <- function(object, ...) {
  ls(object, ...)
}