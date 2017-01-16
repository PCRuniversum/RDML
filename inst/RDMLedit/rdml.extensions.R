format.smpl.name <- function(name, break.every = 20, break.char = "<br/>") {
  gsub(sprintf("(.{%s})", break.every), paste0("\\1", break.char), name)
}

dataType$set("public", "rawAdp",
             NA,
             overwrite = TRUE)

dataType$set("public", "PreprocessAdp",
             function(smooth, smooth.method, normqPCR.method) {
               if (!is.environment(self$rawAdp)) {
                 self$rawAdp <- private$.adp$clone(deep = TRUE)
               }
               private$.adp$fpoints$fluor <- 
                   CPP(self$rawAdp$fpoints$cyc,
                       self$rawAdp$fpoints$fluor,
                       smoother = smooth,
                       method = smooth.method,
                       method.norm = normqPCR.method)[[1]]
             },
             overwrite = TRUE)

dataType$set("public", "UndoPreprocessAdp",
             function() {
               if (!is.environment(self$rawAdp)) {
                 self$rawAdp <- private$.adp$clone(deep = TRUE)
               } else {
                 private$.adp$fpoints$fluor <- 
                   self$rawAdp$fpoints$fluor
               }
               NULL
             },
             overwrite = TRUE)

dataType$set("public", "CalcCq",
             #calculate Ct
             function(cq.method, th.level, auto.th) {
               fdt <- self$adp$fpoints
               switch(cq.method,
                      none = {
                        cq <- self$cq
                        quantFluor <- self$quantFluor
                      },
                      th = {
                        th <- th.cyc(fdt$cyc, fdt$fluor, r = th.level,
                                     auto = auto.th)
                        cq <- th[1, 1]
                        quantFluor <- th[1, 2]
                        if (is.na(cq)) {
                          cq <- NULL
                          quantFluor <- NULL
                        }},
                      sdm = {
                        res <- diffQ2(data.frame(fdt$cyc, fdt$fluor), inder = TRUE, warn = FALSE)
                        
                        cq <-  res$xTm1.2.D2[1]
                        cyc.before.i <- tail(which(fdt$cyc < res$xTm1.2.D2[1]), 1)
                        if (cyc.before.i == length(fdt$cyc)) {
                          quantFluor <- fdt$fluor[cyc.before.i]
                        } else {
                          delta <- (res$xTm1.2.D2[1] - fdt$cyc[cyc.before.i]) / 
                            (fdt$cyc[cyc.before.i + 1] - fdt$cyc[cyc.before.i])
                          quantFluor <- (fdt$fluor[cyc.before.i + 1] - fdt$fluor[cyc.before.i]) *
                            delta + fdt$fluor[cyc.before.i]
                        }
                      })
               self$cq <- cq
               self$quantFluor <- quantFluor
             },
             overwrite = TRUE)