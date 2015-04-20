#' The Names of the RDML object
#' 
#' Just wraps \code{ls()} function to show RDML public fields
#' and methods. Useful for auto complete in R editors.
#' 
#' @aliases names.RDML names,RDML-method
#' @param x an object of class \code{RDML}.
#' @param ... other arguments passed to \code{\link[base]{ls}} function.
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>, Stefan Roediger
#' <stefan.roediger@@b-tu.de>, Michal Burdukiewicz
#' <michalburdukiewicz@@gmail.com>
#' @export
#' @include RDML.R
names.RDML <- function(x, ...) {
  ls(x, ...)
}