#' Plot RDML_objects
#' 
#' Plots \code{RDML} objects.
#' 
#' @aliases plot.RDML_object plot,RDML_object-method
#' @param object an object of class \code{RDML}.
#' @param print.legend \code{logical}
#' @param separate.by \code{list}
#' @param col sth
#' @param empty.col sth
#' @param ... additional graphical parameters.
#' @export

plot.RDML <- function(object,
                             print.legend = TRUE,
                             separate.by = list(left = c("name", "type", "targets"),
                                                right = c("name", "type", "targets")),
                             col = list(left = NA,
                                        right = NA),
                             empty.col = "white",
                             ...) {
  object$Plot(print.legend, separate.by, col, empty.col, ...)
}