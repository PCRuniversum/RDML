plot.RDML_object <- function(object,
                             sort.by = list(colors = c("name", "type", "targets"),
                                            notes = c("name", "type", "targets")),
                             col = "random",
                             notecol = "black",
                             ...) {
  library(digest)
  library(stringr)
  library(gplots)
  matrix.template <- matrix(ncol = object$Plate.Dims["columns"],
                            nrow = object$Plate.Dims["rows"])
  colnames(matrix.template) <- 1:object$Plate.Dims["columns"]
  rownames(matrix.template) <- LETTERS[1:object$Plate.Dims["rows"]]
  plate.matrix <- list(colors = matrix.template,
                       notes = matrix.template)
  
  
  tube.types.template <- matrix(nrow = 4,
                                dimnames = list(c("Hash",
                                                  "Name",
                                                  "Type",
                                                  "Targets")))
  tube.types <- list(colors = tube.types.template,
                     notes = tube.types.template)
  
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
    
    for(var in c("colors", "notes")) {
      
      sname<- ifelse("name" %in% sort.by[[var]],
                           object$Plate.Map[[index]]$Name,
                           NA)
      
      
      ttype <- object$Plate.Map[[index]]$Type
      names(ttype) <- ""
      ttype <- ifelse("type" %in% sort.by[[var]],
                           ttype,
                           NA)
      
      
      targets <- ifelse("targets" %in% sort.by[[var]],
                             paste(object$Plate.Map[[index]]$Targets,
                                   collapse = "; "),
                             NA)
      
      
      tube <- c(sname, ttype, targets)
      
      tube.hash <- digest(tube, algo = "crc32")
      tube.type.index <- which(tube.types[[var]]["Hash",] == tube.hash)
      if(length(tube.type.index) == 0) {      
        if(index == 1) tube.types[[var]][,1] <- c(tube.hash, tube)
        else tube.types[[var]] <- cbind(tube.types[[var]], c(tube.hash, tube))
        tube.type.index <- length(tube.types[[var]]["Hash",])
      }
      plate.matrix[[var]][row.index, col.index] <- tube.type.index
      if(var == "colors")
        legend[index,"Color ID"] <- tube.type.index
      else
        legend[index,"Note ID"] <- tube.type.index
    }
  }
  
  if("random" %in% col) {
    cl <- colors()
    col = cl[runif(length(tube.types$colors["Hash",]), 1, length(cl))]    
  }
  legend[,"Color"] <- sapply(legend[,"Color ID"],
                             function(col.id) {
                               col[as.integer(col.id)]})
  print(legend)
  hmap <- heatmap.2(plate.matrix$colors,
                    Rowv = FALSE,
                    Colv = "Rowv",
                    dendrogram = "none",
                    col = col,            
                    colsep = 0:object$Plate.Dims["columns"],
                    rowsep = 0:object$Plate.Dims["rows"],
                    sepcolor = "black",
                    sepwidth = c(0.01, 0.01),
                    trace = "none",
                    cellnote = plate.matrix$notes,
                    notecol = notecol,
                    key.title = NA,
                    ...)
  res <- list(hmap = list(hmap),
              ff= list("f"))
  invisible(res)
}