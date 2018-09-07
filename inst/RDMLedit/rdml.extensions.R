format.smpl.name <- function(name, break.every = 20, break.char = "<br/>") {
  gsub(sprintf("(.{%s})", break.every), paste0("\\1", break.char), name)
}

dataType$set("public", "rawAdp",
             NA,
             overwrite = TRUE)

dataType$set("public", "PreprocessAdp",
             function(smooth, smooth.method, normqPCR.method) {
               if (!is.environment(self$rawAdp)) {
                 self$rawAdp <- private$.adp$copy()
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
                 self$rawAdp <- private$.adp$copy()
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

dataType$set("public",
             "DetectHook",
             function(hookDetectionMethod, sample, position) {
               
               hookRes <- 
                 if (hookDetectionMethod == "hookreg" || hookDetectionMethod == "both") {
                   as.logical(hookreg(x = self$adp$fpoints[["cyc"]],
                                      y = self$adp$fpoints[["fluor"]])[["hook"]])
                 } else {
                   FALSE
                 }
               hookNLRes <- 
                 if (hookDetectionMethod == "hookregNL" || hookDetectionMethod == "both") {
                   as.logical(hookregNL(x = self$adp$fpoints[["cyc"]],
                                      y = self$adp$fpoints[["fluor"]])[["hook"]])
                 } else {
                   FALSE
                 }
               
               if (hookRes && hookNLRes) {
                 sample$annotation <- 
                   c(sample$annotation, 
                     annotationType$new(sprintf("%s_hookDetectionMethod",position),
                                        "both"))
               } else {
                 if (hookRes) {
                   sample$annotation <- 
                     c(sample$annotation, 
                       annotationType$new(sprintf("%s_hookDetectionMethod",position),
                                          "hookreg"))
                 }
                 if (hookNLRes) {
                   sample$annotation <- 
                     c(sample$annotation, 
                       annotationType$new(sprintf("%s_hookDetectionMethod",position),
                                          "hookregNL"))
                 }
               }
               
               if (hookRes || hookNLRes) {
                 sample$annotation <- 
                   c(sample$annotation, 
                     annotationType$new(sprintf("%s_hook", position),
                                        "TRUE"))
               } else {
                 sample$annotation <- 
                   c(sample$annotation, 
                     annotationType$new(sprintf("%s_hookDetectionMethod",position),
                                        "hookDetectionMethod"))
                 sample$annotation <- 
                   c(sample$annotation, 
                     annotationType$new(sprintf("%s_hook", position),
                                        "FALSE"))
               }
               ""
             },
             overwrite = TRUE)

dataType$set("public", "rawMdp",
             NA,
             overwrite = TRUE)

dataType$set("public", "PreprocessMdp",
             function(bgadj, bg, minmax, df.fact) {
               if (!is.environment(self$rawMdp)) {
                 self$rawMdp <- private$.mdp$copy()
               } 
               private$.mdp$fpoints$fluor <- 
                 mcaSmoother(self$rawMdp$fpoints$tmp,
                             self$rawMdp$fpoints$fluor,
                             bgadj = bgadj,
                             bg = bg,
                             minmax = minmax,
                             df.fact = df.fact)[, 2]
             },
             overwrite = TRUE)

dataType$set("public", "UndoPreprocessMdp",
             function() {
               if (!is.environment(self$rawMdp)) {
                 self$rawMdp <- private$.mdp$copy()
               } else {
                 private$.mdp$fpoints$fluor <- 
                   self$rawMdp$fpoints$fluor
               }
               NULL
             },
             overwrite = TRUE)