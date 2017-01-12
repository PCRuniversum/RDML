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
                        sdm <- inder(x = fdt$cyc, y = fdt$fluor)
                        cq <-  sdm[sdm[, "d2y"] == max(sdm[, "d2y"]), "x"]
                        quantFluor <- sdm[sdm[, "x"] == cq, "y"]
                        })
               self$cq <- cq
               self$quantFluor <- quantFluor
             },
             overwrite = TRUE)