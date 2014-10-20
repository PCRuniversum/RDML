RDML$set("public", "Save.RDML", function() {
  tree <- newXMLNode("rdml", namespace = list(rdml = "http://www.rdml.org", "http://www.rdml.org"))  
  addAttributes(tree, version = "1.2")
  
  exp <- newXMLNode("experiment")
  addAttributes(exp, id = "?")
  
  run <- newXMLNode("run")
  addAttributes(run, id = "1")
  
  pcr.format <- newXMLNode("pcrFormat")
  addChildren(pcr.format, newXMLNode("rows", "8"))
  addChildren(pcr.format, newXMLNode("columns", "12"))
  addChildren(pcr.format, newXMLNode("rowLabel", "ABC"))
  addChildren(pcr.format, newXMLNode("columnLabel", "123"))
  
  react <- newXMLNode("react")
  addChildren(react, newXMLNode("sample", "Sa1"))
  
  data <- newXMLNode("data")
  addChildren(data, newXMLNode("tar", "tar1"))
  adp <- newXMLNode("adp")
  addChildren(adp, newXMLNode("cyc", "1"))
  addChildren(adp, newXMLNode("fluor", "1000"))
  addChildren(data, adp)
  
  addChildren(react, data)
  
  addChildren(run, pcr.format)
  addChildren(run, react)
  addChildren(exp, run)
  addChildren(tree, exp)
}, overwrite = TRUE)