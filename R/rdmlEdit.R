#' RDML Editor Graphical User Interface
#' 
#' Launches graphical user interface that can edit RDML metadata and show
#' qPCR or melting curves.
#' 
#' @keywords hplot
#' @export rdmlEdit
rdmlEdit <- function()
  shiny::runApp(system.file("RDMLedit", package = "RDML"))