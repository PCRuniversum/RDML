#' Merges \code{RDML} objects
#' 
#' Merges list of \code{RDML} objects. The first object in the list becomes base object.
#' If \code{experiments} or \code{runs} have same name they will be combined. 
#' Reacts with same \code{id}, \code{experiment} and \code{run} overwrite each other! 
#' 
#' @param to.merge \code{RDML} objects that should be merged.
#' 
#' @docType methods
#' @name MergeRDMLs
#' @include RDML.R
#' @export
#' @examples
#' \dontrun{
#' PATH <- path.package("RDML")
#' filename <- paste(PATH, "/extdata/", "lc96_bACTXY.rdml", sep ="")
#' lc96 <- RDML$new(filename)
#' filename <- paste(PATH, "/extdata/", "stepone_std.rdml", sep ="")
#' stepone <- RDML$new(filename)
#' merged <- MergeRDMLs(list(lc96,stepone))
#' merged$AsDendrogram()
#' }
MergeRDMLs <- function(to.merge) {
  baseRDML <- to.merge[[1]]$clone(deep = TRUE)
  for (rdml in to.merge[-1]) {
    for (element in c("id",
                      "experimenter",
                      "documentation",
                      "dye",
                      "sample",
                      "target",
                      "thermalCyclingConditions"
                      # ,"experiment"
    )) {
      if (length(rdml[[element]]) != 0) {
        baseRDML[[element]] <- c(baseRDML[[element]],
                                 list.map(rdml[[element]],
                                          subelement ~
                                            subelement$copy()
                                 ))
        # leave only unique subelements
        baseRDML[[element]] <- baseRDML[[element]][unique(names(baseRDML[[element]]))]
      }
    }
    # merge experiments
    for (exp.name in names(rdml[["experiment"]])) {
      # if experiment have unique name just add. Else test run names.
      if (exp.name %in% names(baseRDML[["experiment"]])) {
        for (run.name in names(rdml[["experiment"]][[exp.name]][["run"]])) {
          # if experiment have unique name just add. Else combine reacts to one run.
          if (run.name %in% names(baseRDML[["experiment"]][[exp.name]][["run"]])) {
            baseRDML[["experiment"]][[exp.name]][["run"]][[run.name]][["react"]] <- 
              c(baseRDML[["experiment"]][[exp.name]][["run"]][[run.name]][["react"]],
                list.map(rdml[["experiment"]][[exp.name]][["run"]][[run.name]][["react"]],
                         subelement ~
                           subelement$copy()
                ))
          } else {
            baseRDML[["experiment"]][[exp.name]][["run"]] <-
              c(baseRDML[["experiment"]][[exp.name]][["run"]],
                rdml[["experiment"]][[exp.name]][["run"]][[run.name]]$copy()
              )
          }
        }  
      } else {
        baseRDML[["experiment"]] <- c(baseRDML[["experiment"]],
                                      rdml[["experiment"]][[exp.name]]$copy()
        )
      }
    }
  }
  baseRDML
}