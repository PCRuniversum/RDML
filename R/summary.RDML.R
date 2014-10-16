#' Summary RDML R6 objects
#' 
#' Prints and silently returns summary statistics of \code{RDML} objects.
#' 
#' 
#' @aliases summary.RDML_object summary,RDML_object-method
#' @param object an object of class \code{RDML_object}.
#' @param print logical, if \code{TRUE} summary is printed to console.
#' @param ...  currently ignored.
#' @return A list of length at most 3 (less if \code{object} does not contain
#' information about dilutions, qPCR or melting data): \item{dilTable}{A list
#' of dilutions used in experiments.} \item{meltTable}{Melting data - currently
#' not implemented.} \item{expTable}{Summary statistics describing experiments:
#' mean, median, standard deviation, median absolute deviation, interquartile
#' range, medcouple, skewness, SNR, VRM, number of NAs (missing values),
#' intercept, slope, R squared, Breusch-Pagan test p-value.}
#' @author Michal Burdukiewicz, Konstantin A. Blagodatskikh, Stefan Roediger
#' @seealso Summary statistics are calculated using
#' \code{\link[chipPCR]{MFIaggr}}.
#' @keywords manip summary
#' @examples
#' 
#' lc96 <- RDML$new(system.file("extdata/lc96_bACTXY.rdml", package = "RDML"))
#' summary(lc96)
#' #store raw results of summary without printing
#' res <- summary(lc96, print = FALSE)
#' 
summary.RDML <- function(object, print = TRUE, ...) {
  object$CreateSummary(print, ...)
}