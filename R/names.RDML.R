#' The Names of the RDML object
#' 
#' Just wraps \code{ls()} function to show RDML public fields
#' and methods. Useful for RStudio autocomplete.
#' 
#' @aliases names.RDML names,RDML-method
#' @param object an object of class \code{RDML}.
#' @export
#' @include RDML.R
names.RDML <- function(object) {
  ls(object)
}