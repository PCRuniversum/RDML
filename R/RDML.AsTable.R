#' @include RDML.R
RDML$set("public", "AsTable",
         function(.default = list(
           exp.id = experiment$id,
           run.id = run$id,
           react.id = react$id,
           position = react$position,
           sample = react$sample,
           sample.description = private$.sample[[react$sample]]$description,
           target = data$id,
           target.dyeId = private$.target[[data$id]]$dyeId,
           sample.type=private$.sample[[react$sample]]$type),
           name.pattern = paste(
             react$id,
             react$sample,
             private$.sample[[react$sample]]$type,
             data$id,
             sep = "_"),                    
           ...) {
           # create short names
           dateMade <- private$.dateMade
           dateUpdated <- private$.dateUpdated
           id <- private$.id
           experimenter <- private$.experimenter
           documentation <- private$.documentation
           dye <- private$.dye
           sample <- private$.sample
           target <- private$.target
           thermalCyclingConditions <- private$.thermalCyclingConditions
           
           out<-data.frame()
           for(experiment in private$.experiment) {                      
             for(run in experiment$run) {                                    
               for(react in run$react) {                          
                 for(data in react$data){
                   out<-rbind(out,
                              data.frame(
                                eval(substitute(list(.default, ...))),
                                row.names = eval(substitute(name.pattern)),
                                stringsAsFactors = FALSE
                              )
                   )
                 }
               }
             }
           }
           out
         }, 
         overwrite = TRUE)