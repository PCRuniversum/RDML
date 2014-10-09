#' Graphical User Interface for .rdml data import
#' 
#' \code{RDML_gui} is a web-browser based graphical interface allowing user to
#' read \code{.rdml} file, extract data and save it in an other format.
#' Additional features include summary statistics and plotting functions.
#' 
#' 
#' @section Warning : Any ad-blocking software may cause of malfunctions.
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>, Stefan Roediger
#' <stefan.roediger@@hs-lausitz.de>, Michal Burdukiewicz
#' <michalburdukiewicz@@gmail.com>
#' @seealso Read \code{.rdml} files: \code{\link{RDML}}. Select data from
#' \code{.rdml} files: \code{\link{selectFData}}. Summary statistics:
#' \code{\link{summary.RDML_object}}.
#' @keywords hplot shiny GUI browser
#' @export
#' @examples
#' 
#' \donttest{
#' library(shiny)
#' RDML_gui()
#' }
#' 
RDML_gui <- function()
  runApp(system.file("RDML_gui", package = "RDML"))
