plot.RDML_object <- function(object,
                             print.legend = TRUE,
                             separate.by = list(left = c("name", "type", "targets"),
                                            right = c("name", "type", "targets")),
                             col = NA,
                             ...) {
  ## create matrix for type and notes values of tubes
  matr <- matrix(ncol = 2,
                 nrow = object$Plate.Dims["columns"] * 
                   object$Plate.Dims["rows"])
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
  legend <- matrix(ncol = 6, nrow = length(object$Plate.Map),
                   dimnames = list(c(), c("Color ID",
                                          "Color",
                                          "Note ID",
                                          "Tube Name",
                                          "Type",
                                          "Targets")))
  
  for(index in 1:length(object$Plate.Map)) {
    col.index <- as.integer(str_extract(names(object$Plate.Map)[index],
                                        "[[:digit:]]+"))
    row.index <- which(LETTERS == 
                         str_extract(names(object$Plate.Map)[index], 
                                     "[[:upper:]]"))
    legend[index,"Tube Name"] <- object$Plate.Map[[index]]$Name
    legend[index,"Type"] <- object$Plate.Map[[index]]$Type
    legend[index,"Targets"] <- paste(object$Plate.Map[[index]]$Targets,
                                       collapse = "; ")
    
    for(var in c("left", "right")) {      
      sname<- ifelse("name" %in% separate.by[[var]],
                           object$Plate.Map[[index]]$Name,
                           NA)
      ttype <- object$Plate.Map[[index]]$Type
      ## remove variable name
      names(ttype) <- ""
      ttype <- ifelse("type" %in% separate.by[[var]],
                           ttype,
                           NA)
      ## collapse all targets to one string
      targets <- ifelse("targets" %in% separate.by[[var]],
                             paste(object$Plate.Map[[index]]$Targets,
                                   collapse = "; "),
                             NA)    
      
      tube <- c(sname, ttype, targets)
      ## generate hash of tube
      tube.hash <- digest(tube, algo = "crc32")
      
      ## test if tube with given hash already exists and
      ## If not exists - add tube to tube.types matrix
      tube.type.index <- which(tube.types[[var]]["Hash",] == tube.hash)
      if(length(tube.type.index) == 0) {      
        if(index == 1) tube.types[[var]][,1] <- c(tube.hash, tube)
        else tube.types[[var]] <- cbind(tube.types[[var]], c(tube.hash, tube))
        tube.type.index <- length(tube.types[[var]]["Hash",])
      }
      
      ## add type of tube (index of tube hash at tube.types) 
      ## matr
      matr[row.index + col.index * 8, var] <- tube.type.index + 1
      
      ## add type off tube (index of tube hash at tube.types) 
      ## to legend
      if(var == "left")
        legend[index,"Color ID"] <- tube.type.index
      else
        legend[index,"Note ID"] <- tube.type.index
    }
  }
  
  ## generate random colors for cells
  col.is.not.closure <- TRUE
  tryCatch({
    if(typeof(get(col)) == "closure") col.is.not.closure <- FALSE
    },
    error = function(e) {}
    )
  if(is.na(col) || col == "") col <- NA
  if(is.na(col) || (length(col) < length(tube.types$left["Hash",]) &&
       col.is.not.closure)) {
    cl <- colors()
    if(!is.na(col)) {
      col = c(col, cl[runif(length(tube.types$left["Hash",]) - length(col),
                            1, length(cl))])
    }
    else {
      col = cl[runif(length(tube.types$left["Hash",]),
                     1, length(cl))]
    }
  }  
  
  ## add used colors to legend
  legend[,"Color"] <- sapply(legend[,"Color ID"],
                             function(col.id) {
                               col[as.integer(col.id)]})  
  
#   # heatmap.2 doesn't work with one color!!
#   # adding one color works for plot,
#   # but breaks "key"
#   if(length(col) == 1 && col.is.not.closure) {
#     col <- c(col, "red")
#     key <- FALSE
#   }
  
  col = c("white" , col)
  ## draw plate
#   hmap <- heatmap.2(plate.matrix$colors,
#                     Rowv = FALSE,
#                     Colv = "Rowv",
#                     dendrogram = "none",
#                     col = col,            
#                     colsep = 0:object$Plate.Dims["columns"],
#                     rowsep = 0:object$Plate.Dims["rows"],
#                     sepcolor = "black",
#                     sepwidth = c(0.01, 0.01),
#                     trace = "none",
#                     cellnote = plate.matrix$notes,
#                     notecol = notecol,
#                     key = key,
#                     key.title = NA,
#                     ...)
  matr[is.na(matr)] <- 0
  matr.dpcr <- create_dpcr(matr, n = 1L, type = "nm") 
  plot_panel(extract_dpcr(matr.dpcr, "left"), nx_a = object$Plate.Dims["columns"],
             ny_a = object$Plate.Dims["rows"],
             col = col,
             legend = FALSE, 
             half = "left")
  par(new = TRUE)
  plot_panel(extract_dpcr(matr.dpcr, 2), nx_a = object$Plate.Dims["columns"],
             ny_a = object$Plate.Dims["rows"],             
             legend = FALSE, half = "right")
  
  legend <- as.data.frame(legend)
  
  if(print.legend) {
    cat("\nPlot legend:\n")
    print(legend)
    cat("\n")
  }
  
#   res <- list(Hmap = hmap,
#               Legend = legend)
#   invisible(res)
}