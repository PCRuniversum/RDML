RDML <- R6Class("RDML",
                   public = list(                
                     plate.name = NA,
                     publisher = NA,
                     dilutions = NA,
                     plate.dims = NA,
                     
                     initialize = function(file.name,
                                           name.pattern = "%NAME%__%TUBE%") {
                       init <- InitRDML(self, private, file.name)
                       self <- init[["self"]]
                       private <- init[["private"]]
                       self$SetNamePattern(name.pattern)
                     },
                     SetNamePattern = function(name.pattern) {
                       private$.name.pattern <- name.pattern
                       private$plate.map["FDataName", ] <- 
                         sapply(colnames(private$plate.map),
                                function(cname) {
                                  GenFDataName(private$.name.pattern,
                                               private$plate.map["ReactID", cname],
                                               private$plate.map["TubeName", cname],
                                               private$plate.map["Tube", cname],
                                               private$plate.map["Target", cname],
                                               private$plate.map["Type", cname]
                                  )
                                }
                         )
                     },
                     GetFData = function(i) {
                       print(private$plate.map[,i])                       
                       print(private$qPCR.fdata[,i])
                       print(private$melt.fdata[,i])
                     }
                   ),
                   private = list(
                     qPCR.fdata = NA,                  
                     melt.fdata = NA,
                     plate.map = NA,
                     .name.pattern = NA
                   ),
                   active = list(
                     targets = function() { NULL
                                            
                     },
                     types = function(target = NA) { NULL
                                                     
                     },
                     name.pattern = function() private$.name.pattern,
                     qd = function() private$qPCR.fdata,
                     md = function() private$melt.fdata,
                     pm = function() private$plate.map
                   )
)