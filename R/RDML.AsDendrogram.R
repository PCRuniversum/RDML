#' Represents structure of \code{RDML} file as dendrogram
#' 
#' Plots and/or returns structure of \code{RDML} file as dendrogram (tree view)
#' 
#' @param plot.dendrogram plots dendrogram if \code{TRUE}
#' @return \code{dendrogram} object
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>, Stefan Roediger 
#'   <stefan.roediger@@b-tu.de>, Michal Burdukiewicz 
#'   <michalburdukiewicz@@gmail.com>
#' @keywords manip
#' @docType methods
#' @name AsDendrogram
#' @aliases RDML.AsDendrogram
#' @rdname asdendrogram-method
#' @include RDML.R
#' @examples
#' PATH <- path.package("RDML")
#' filename <- paste(PATH, "/extdata/", "BioRad_qPCR_melt.rdml", sep ="")
#' cfx96 <- RDML$new(filename)
#' cfx96$AsDendrogram()
RDML$set("public", "AsDendrogram",
         function(plot.dendrogram = TRUE) {
           
           total.table <- self$AsTable(adp = !all(is.na(data$adp)),
                                       mdp = !all(is.na(data$mdp)))
           tree<-list()
           attributes(tree)<-list(members = 0, height = 5)
           class(tree)<-"dendrogram"
           for(exper.id in unique(total.table$exp.id)) {
             
             tree[[exper.id]] <- list()
             attributes(tree[[exper.id]]) <- list(members = 0,
                                                  height = 4,
                                                  edgetext = exper.id)
             for(r.id in total.table %>% 
                 filter(exp.id == exper.id) %>% 
                 .$run.id %>% unique) {
               tree[[exper.id]][[r.id]] <- list()
               attributes(tree[[exper.id]][[r.id]]) <- 
                 list(members = 0,
                      height = 3,
                      edgetext = r.id)
               for(trgt in total.table %>% 
                   filter(exp.id == exper.id,
                          run.id == r.id) %>% 
                   .$target %>% unique) {
                 tree[[exper.id]][[r.id]][[trgt]] <- list()
                 
                 attributes(tree[[exper.id]][[r.id]][[trgt]]) <- 
                   list(members = 0,
                        height = 2,
                        edgetext = trgt)
                 for(stype in total.table %>% 
                     filter(exp.id == exper.id,
                            run.id == r.id,
                            target == trgt) %>% 
                     .$sample.type %>% unique) {
                   tree[[exper.id]][[r.id]][[trgt]][[stype]] <- list()
                   attributes(tree[[exper.id]][[r.id]][[trgt]][[stype]]) <- 
                     list(members = 0,
                          height = 1,
                          edgetext = stype)
                   for(exp.type in c("adp", "mdp")) {
                     n.rows <- total.table %>% 
                       filter(exp.id == exper.id,
                              run.id == r.id,
                              target == trgt,
                              sample.type == stype) %>% 
                       filter_(sprintf("%s == TRUE", exp.type)) %>% 
                       nrow
                     if(n.rows == 0) next()
                     tree[[exper.id]][[r.id]][[trgt]][[stype]][[exp.type]] <- list()
                     attributes(tree[[exper.id]][[r.id]][[trgt]][[stype]][[exp.type]]) <- 
                       list(members = 1,
                            height = 0,
                            edgetext = exp.type,
                            label = n.rows,
                            leaf = TRUE)
                     
                     attributes(tree[[exper.id]][[r.id]][[trgt]][[stype]])$members <- 
                       attributes(tree[[exper.id]][[r.id]][[trgt]][[stype]])$members + 1
                     
                     attributes(tree[[exper.id]][[r.id]][[trgt]])$members <- 
                       attributes(tree[[exper.id]][[r.id]][[trgt]])$members + 1
                     
                     attributes(tree[[exper.id]][[r.id]])$members <- 
                       attributes(tree[[exper.id]][[r.id]])$members + 1
                     
                     attributes(tree[[exper.id]])$members <- 
                       attributes(tree[[exper.id]])$members + 1
                     
                     attributes(tree)$members <- 
                       attributes(tree)$members + 1
                   }
                 }
               }
             }
           }
           if(plot.dendrogram) {
             suppressWarnings(plot(rev(tree),
                                  center=TRUE,
                                  horiz=TRUE,
                                  yaxt="n"))
             xtick<-seq(0, 5, by=0.5)
             axis(side=1,
                  at=xtick,
                  lty = "blank",
                  las = 2,
                  labels = FALSE)
             text(seq(0, 5, by=0.5),
                  par("usr")[3] - 0.2,
                  labels = c("Number\nof samples",
                             "Data type",
                             "",
                             "Sample\ntype",
                             "",
                             "Target\n(gene)",
                             "",
                             "Run ID",
                             "",
                             "Experiment ID",
                             ""),
                  srt = 45, pos = 1, xpd = TRUE)
           }
           return(tree)},
         overwrite = TRUE)