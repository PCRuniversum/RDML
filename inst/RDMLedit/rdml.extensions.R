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
             function(ct.range, fluo.limit, th, rdml, kit) {
               dat <- private$.adp$fpoints %>>%
                 as.data.frame %>>%
                 select(cyc, fluor)
               #move tr calculation to the right of baseline
               
               if (ct.range[["from"]] < self$bgTo)
               {
                 ct.range[["from"]] <- self$bgTo + 1
                 
               }
               
               if (tail(dat[, "cyc"], 1) < ct.range[["to"]])
                 ct.range[["to"]] <- tail(dat[, "cyc"], 1)
               if (max(dat[, "fluor"][ct.range[["from"]]:ct.range[["to"]]]) >= (fluo.limit)) {
                 Ct <- {
                   my.th.cyc(th,
                             dat %>>%
                               filter(findInterval(cyc, unlist(ct.range)) == 1))
                   # filtered.points <- dat %>>%
                   #   filter(findInterval(cyc, unlist(ct.range)) == 1)
                   # tryCatch(
                   #   th.cyc(filtered.points$cyc, filtered.points$fluor, r = th, linear = FALSE)@
                   #     .Data[1, 1],
                   #   error = function(e) NA
                   # )
                 }
                 print(paste0("Ct=",Ct,"bgTo= ",self$bgTo))
               } else { # below fluo limit
                 # warning("Maximal fluorescence below limit")
                 Ct <- NA
               }
               
               private$.cq <- Ct
             },
             overwrite = TRUE)