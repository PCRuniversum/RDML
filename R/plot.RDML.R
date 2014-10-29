#' S3 wrapper for \code{RDML$Plot()} method
#' 
#' See \link{RDML.Plot}.
#' 
#' @export
#' @include RDML.R
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

#' Plots \code{RDML} object
#' 
#' Plots \code{RDML} objects.
#' 
#' @param print.legend \code{logical} defines print or not plot legend
#' @param separate.by \code{list}
#' @param col sth
#' @param empty.col sth
#' @param ... additional graphical parameters.
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>, Stefan Roediger 
#'   <stefan.roediger@@hs-lausitz.de>, Michal Burdukiewicz 
#'   <michalburdukiewicz@@gmail.com>
#' @docType methods
#' @name Plot
#' @aliases RDML.Plot
#' @rdname plot-method
RDML$set("public", "Plot", function(print.legend = TRUE,
                                    separate.by = list(left = c("name", "type", "targets"),
                                                       right = c("name", "type", "targets")),
                                    col = list(left = NA,
                                               right = NA),
                                    empty.col = "white",
                                    ...) {
  for(half in c("left", "right")) {
    for(opt in separate.by[[half]]) {      
      if(!opt %in%  c("name", "type", "targets", "", NA))
        stop(paste0(c("'", opt, "' is not correct option for separate.by!!!")))
    }
  }
  cnum = private$.plate.dims["columns"]
  rnum = private$.plate.dims["rows"]
  tubes <- unique(private$.plate.map$Tube)
  ## create matrix for left and right values of tubes
  matr <- matrix(ncol = 2,
                 nrow = cnum * rnum)
  colnames(matr) <- c("left", "right")
  
  ## create matrices for types of tubes (defined by separate.by
  ## parametres) at plate 
  tube.types.template <- matrix(nrow = 4,
                                dimnames = list(c("Hash",
                                                  "Name",
                                                  "Type",
                                                  "Targets")))
  tube.types <- list(left = tube.types.template,
                     right = tube.types.template)
  
  ## create matrix for plot legend
  legend <- matrix(ncol = 7, nrow = length(tubes),
                   dimnames = list(c(), c("Left ID",
                                          "L. Color",
                                          "Right ID",
                                          "R. Color",
                                          "Tube Name",
                                          "Type",
                                          "Targets")))
  
  for(tube in tubes) {
    col.index <- as.integer(str_extract(tube,
                                        "[[:digit:]]+"))
    row.index <- which(LETTERS == 
                         str_extract(tube, 
                                     "[[:upper:]]"))
    
    plate.map.by.tube <- private$.plate.map[private$.plate.map$Tube == tube,]    
    index <- which(tubes == tube)    
    legend[index,"Tube Name"] <- as.character(plate.map.by.tube$TubeName[1])
    legend[index,"Type"] <- as.character(plate.map.by.tube$Type[1])
    legend[index,"Targets"] <- paste(plate.map.by.tube$Target,
                                     collapse = "; ")
    
    for(half in c("left", "right")) {
      sname<- ifelse("name" %in% separate.by[[half]],
                     plate.map.by.tube$TubeName[1],
                     NA)
      ttype <- plate.map.by.tube$Type[1]
      ## remove variable name
      names(ttype) <- ""
      ttype <- ifelse("type" %in% separate.by[[half]],
                      ttype,
                      NA)
      ## collapse all targets to one string
      targets <- ifelse("targets" %in% separate.by[[half]],
                        paste(plate.map.by.tube$Target,
                              collapse = "; "),
                        NA)    
      
      ttube <- c(sname, ttype, targets)
      ## generate hash of tube
      tube.hash <- digest(ttube, algo = "crc32")
      
      ## test if tube with given hash already exists and
      ## If not exists - add tube to tube.types matrix
      tube.type.index <- which(tube.types[[half]]["Hash",] == tube.hash)
      if(length(tube.type.index) == 0) {        
        if(tube == tubes[1]) tube.types[[half]][,1] <- c(tube.hash, ttube)
        else tube.types[[half]] <- cbind(tube.types[[half]], 
                                         c(tube.hash, ttube))
        tube.type.index <- length(tube.types[[half]]["Hash",])
      }
      
      ## add type of tube (index of tube hash at tube.types) 
      ## matr
      matr[row.index + (col.index - 1)  * rnum, half] <- tube.type.index        
      
      ## add type off tube (index of tube hash at tube.types) 
      ## to legend
      if(half == "left")
        legend[index,"Left ID"] <- tube.type.index
      else
        legend[index,"Right ID"] <- tube.type.index
      
    }
    
  }
  ## generate random colors for cells
  for(half in c("left", "right")) {
    if(is.na(col[[half]]) || col[[half]] == "") col[[half]] <- NA
    if(is.na(col[[half]]) || 
         (length(col[[half]]) < length(tube.types$left["Hash",]) )) {
      cl <- colors()
      if(!is.na(col[[half]])) {
        col[[half]] <- c(col[[half]], cl[runif(length(tube.types[[half]]["Hash",]) - length(col[[half]]),
                                               1, length(cl))])
      }
      else {
        col[[half]] <- cl[runif(length(tube.types[[half]]["Hash",]),
                                1, length(cl))]
      }
    }
    ## add used colors to legend
    if(half == "left") 
      legend[,"L. Color"] <- sapply(legend[,"Left ID"],
                                    function(col.id) {
                                      col[[half]][as.integer(col.id)]})
    else legend[,"R. Color"] <- sapply(legend[,"Right ID"],
                                       function(col.id) {
                                         col[[half]][as.integer(col.id)]})
    col[[half]] <- c(empty.col , col[[half]])
  }
  
  matr[is.na(matr)] <- 0
  matr.dpcr <- create_dpcr(matr, n = 1L, type = "nm")
  
  ########## plotting
  ## plot left halfs of tubes
  plot_panel(extract_dpcr(matr.dpcr, "left"), 
             nx_a = cnum,
             ny_a = rnum,
             col = col[["left"]],
             legend = FALSE, 
             half = "left",
             use_breaks = FALSE,
             ...)
  par(new = TRUE)
  ## plot right halfs of tubes
  plot_panel(extract_dpcr(matr.dpcr, "right"),
             nx_a = cnum,
             ny_a = rnum,
             col = col[["right"]],
             legend = FALSE,
             half = "right",
             use_breaks = FALSE,
             ...)
  ## add axes
  box()
  # cols
  axis(side = 3, at = 1:cnum, labels = 1:cnum, ...)
  # rows
  axis(side = 2, at = 1:rnum, labels = rev(LETTERS[1:rnum]),
       las = 1, # the style of axis labels: always horizontal.
       ...)
  ########## end plotting
  
  legend <- as.data.frame(legend)
  
  if(print.legend) {
    cat("\nPlot legend:\n")
    print(legend)
    cat("\n")
  }  
  
  invisible(legend)
}
, overwrite = TRUE)
