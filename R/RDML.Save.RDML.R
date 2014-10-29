RDML$set("public", "Save.RDML", function(file.name = NULL) {
  ### header
  tree.node <- newXMLNode("rdml",
                          namespace = list(rdml = "http://www.rdml.org", "http://www.rdml.org"))  
  addAttributes(tree.node, version = "1.2")
  ### dyes
  for(dye in unique(private$.plate.map[["Dye"]])) {
    addChildren(tree.node,
                newXMLNode("dye", attrs = list(id = dye)))
  }
  
  ### samples
  samples.names <- unique(private$.plate.map["TubeName"])
  for(id in rownames(samples.names)) {    
    sample.node <- newXMLNode("sample")
    # sample name
    addAttributes(sample.node,
                  id = as.character(samples.names[as.character(id),]))  
    # sample type    
    sample.type <- private$.plate.map[id, "Type"]    
    addChildren(sample.node, newXMLNode("type", sample.type))
    # sample quantity
    sample.tube <- private$.plate.map[id, "Tube"]
    sample.quantity <- private$.dilutions[[1]][as.character(sample.tube)]
    if(!is.null(sample.quantity)) {
      quantity.node <- newXMLNode("quantity")
      addChildren(quantity.node, newXMLNode("value",
                                            sample.quantity))
      addChildren(quantity.node, newXMLNode("unit",
                                            "ng")) # ng!!!
      addChildren(sample.node, quantity.node)
    }
    # add sample to tree
    addChildren(tree.node, sample.node)
  }
  
  # targets
  targets.ids <- unique(private$.plate.map["Target"])
  for(id in rownames(targets.ids)) {
    dye <- as.character(private$.plate.map[id, "Dye"])
    target <- as.character(private$.plate.map[id, "Target"])
    addChildren(tree.node,
                newXMLNode("target",
                           newXMLNode("dyeId",
                                      attrs = list(id = dye)),
                           attrs = list(id = target)))
  }
  
  exp.node <- newXMLNode("experiment")
  addAttributes(exp.node, id = "exp1")
  
  run.node <- newXMLNode("run")
  addAttributes(run.node, id = "run1")
  
  ### plate format
  pcr.format.node <- newXMLNode("pcrFormat")
  addChildren(pcr.format.node, newXMLNode("rows",
                                          private$.plate.dims["rows"]))
  addChildren(pcr.format.node, newXMLNode("columns",
                                          private$.plate.dims["columns"]))
  addChildren(pcr.format.node, newXMLNode("rowLabel", "ABC"))
  addChildren(pcr.format.node, newXMLNode("columnLabel", "123"))
  addChildren(run.node, pcr.format.node)
  
  ### add reacts
  react.ids <- unique(private$.plate.map[["ReactID"]])
  for(id in react.ids) {
    react.node <- newXMLNode("react")
    addAttributes(react.node, id = id)
    react <- private$.plate.map[which(private$.plate.map["ReactID"] == id),]
    # sample name to react  
    sample.name <- react[1, "TubeName"]        
    addChildren(react.node, newXMLNode("sample",
                                       attrs = list(id = as.character(sample.name))))
    # daat by targets
    for(target.i in 1:length(react[["Target"]])) {
      data.node <- newXMLNode("data")
      addChildren(data.node, 
                  newXMLNode("tar",
                             attrs = list(id = as.character(react[["Target"]][target.i]))))      
      fdata.id <- react[target.i, "FDataID"]
      if(!is.null(private$.qPCR.fdata)) {        
        adps <- private$.qPCR.fdata[, fdata.id]        
        for(adp.i in 1:length(adps)) {
          adp.node <- newXMLNode("adp")          
          addChildren(adp.node, newXMLNode("cyc", names(adps[adp.i])))
          addChildren(adp.node, newXMLNode("fluor", adps[adp.i]))
          addChildren(data.node, adp.node)
        }        
      }
      if(!is.null(private$.melt.fdata)) {        
        mdps <- private$.melt.fdata[, fdata.id]        
        for(mdp.i in 1:length(mdps)) {
          mdp.node <- newXMLNode("mdp")          
          addChildren(mdp.node, newXMLNode("tmp", names(mdps[mdp.i])))
          addChildren(mdp.node, newXMLNode("fluor", mdps[mdp.i]))
          addChildren(data.node, mdp.node)
        }        
      }
      addChildren(react.node, data.node)
    }
    addChildren(run.node, react.node)
  }
  
  addChildren(exp.node, run.node)
  addChildren(tree.node, exp.node)
  if(is.null(file.name))
    return(tree.node)
  else {
    uniq.folder <- paste0(".temp/", UUIDgenerate())
    tryCatch({
      dir.create(uniq.folder)
      file <- saveXML(tree.node, file = paste0(uniq.folder,
                                               "/rdml_data.xml"))
      zip(file.name, file, extras = "-j")
    },
    error = function(e) { print(e) },
    finally = unlink(uniq.folder, recursive = TRUE)
    )    
  }
    
}, overwrite = TRUE)