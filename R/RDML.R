# Unzips RDML to get inner XML content
getRDMLdoc <- function(file)
{
  unzippedRDML <- unzip(file)
  if (length(unzippedRDML) > 1)
  {
    rdmldoc <- xmlParse("rdml_data.xml")
  }
  else
  {
    rdmldoc <- xmlParse(unzippedRDML)
  }
  unlink(unzippedRDML)
  return(rdmldoc)
}

# Gets file publisher (instrumant manufacturer)
getPublisher <- function(RDMLdoc)
{
  publisher <- xpathSApply(
    RDMLdoc, 
    "/rdml:rdml/rdml:id/rdml:publisher",
    xmlValue,
    namespaces=c(rdml = "http://www.rdml.org"))  
  if (length(publisher) != 0){ return(publisher) }
  else { return("StepOne") }
}

# Gets PCR targets vector from XML
getTargets <- function(RDMLdoc)
{
  xpathSApply(
    RDMLdoc, 
    "/rdml:rdml/rdml:target[@id]",
    xmlGetAttr, 
    name="id",
    namespaces=c(rdml = "http://www.rdml.org"))
}

# Gets PCR samples descriptions vector from XML
getDescriptions <- function(RDMLdoc)
{
  samplesids <- xpathSApply(
    RDMLdoc, 
    "/rdml:rdml/rdml:sample",
    xmlGetAttr,
    name="id",
    namespaces=c(rdml = "http://www.rdml.org"))
  descriptions <- xpathSApply(
    RDMLdoc, 
    "/rdml:rdml/rdml:sample/rdml:description",
    xmlValue,
    namespaces=c(rdml = "http://www.rdml.org"))  
  names(descriptions) <- samplesids
  return(descriptions)
}

# Gets PCR targets vector from XML
getRuns <- function(RDMLdoc)
{
  xpathSApply(
    RDMLdoc, 
    "/rdml:rdml/rdml:experiment/rdml:run",
    xmlGetAttr, 
    name="id",
    namespaces=c(rdml = "http://www.rdml.org"))
}

# Gets plate dimensions from XML
getPlateDimensions <- function(RDMLdoc)
{
  
  rows <- tryCatch({as.integer(xpathSApply(
    RDMLdoc, 
    "/rdml:rdml/rdml:experiment/rdml:run/rdml:pcrFormat/rdml:rows",
    xmlValue,
    namespaces=c(rdml = "http://www.rdml.org"))[1])},
    error = function(e) 8)
  columns <- tryCatch({as.integer(xpathSApply(
    RDMLdoc, 
    "/rdml:rdml/rdml:experiment/rdml:run/rdml:pcrFormat/rdml:columns",
    xmlValue,
    namespaces=c(rdml = "http://www.rdml.org"))[1])},
    error = function(e) 12)
  return(c(rows = rows, columns = columns))
}

# Gets type (Unknown, Positive, Standart, etc.)
# of each sample from XML
getTypes <- function(RDMLdoc)
{    
  samplesids <- xpathSApply(
    RDMLdoc, 
    "/rdml:rdml/rdml:sample",
    xmlGetAttr,
    name="id",
    namespaces=c(rdml = "http://www.rdml.org"))
  types <- xpathSApply(
    RDMLdoc, 
    "/rdml:rdml/rdml:sample/rdml:type",
    xmlValue,
    namespaces=c(rdml = "http://www.rdml.org"))  
  names(types) <- samplesids
  return(types)
}

# Gets concentrations (quantity) of each 
# dilution from XML
getDilutions <- function(RDMLdoc)
{
  # XML nodes that contain "quantity" information
  nodes <- getNodeSet(
    RDMLdoc, 
    "/rdml:rdml/rdml:sample/rdml:quantity/rdml:value/../..",    
    namespaces=c(rdml = "http://www.rdml.org"))
  values <- sapply(nodes, function(node) {
    as.numeric(xmlValue(node[["quantity"]][["value"]]))})
  # names of the samples that contain "quantity" information
  samplesids <- sapply(nodes, xmlGetAttr, name="id")
  names(values) <- samplesids
  # sorting quantities by sample name
  values<-values[order(names(values))]
  return(values)  
}  


# Get dilutions Roche
getDilutionsRoche <- function(filename)
{
  unzippedRDML <- unzip(filename)
  rdmldoc <- xmlParse("calculated_data.xml")
  unlink(unzippedRDML) 
  
  nodes<- getNodeSet(
    rdmldoc, 
    "//ns:absQuantDataSource/ns:standard/..",    
    namespaces=c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"))
  
  dilutions <- list()
  for(node in nodes){
    quant <- xmlValue(node[["standard"]])
    position <- xpathSApply(
      rdmldoc, 
      paste("//ns:standardPoints/ns:standardPoint/ns:graphIds/ns:guid[text() ='",
            xmlValue(node[["graphId"]]), "']/../../ns:position",
            sep = ""),
      xmlValue,
      namespaces=c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"))
    dye<- xpathSApply(
      rdmldoc, 
      paste("//ns:standardPoints/ns:standardPoint/ns:graphIds/ns:guid[text() ='",
            xmlValue(node[["graphId"]]), "']/../../ns:dyeName",
            sep = ""),
      xmlValue,
      namespaces=c(ns = "http://www.roche.ch/LC96AbsQuantCalculatedDataModel"))
    dilutions[[dye]] <-cbind(dilutions[[dye]],c(position, quant))
  }
  dilutions<-lapply(dilutions, function(dilution){
    quant <- as.numeric(dilution[2, ])
    quant <- t(as.data.frame(quant))
    colnames(quant) <- dilution[1, ]
    return(quant) })
  return(dilutions)
}

# Generates sample name by specified pattern
generateSampleName <- function (name.pattern,
                                plateDims,
                                reactID,
                                tubeName,
                                target,
                                type,
                                publisher)
{   
  tube <- ifelse((publisher == "Roche Diagnostics" || 
                 publisher == "StepOne"),
                 reactID,
                 { reactID <- as.integer(reactID)
                 paste(LETTERS[reactID %/% plateDims["columns"] + 1],
              reactID %% plateDims["columns"], sep="")})  
  name.pattern <- gsub("%NAME%",
                       tubeName,
                       name.pattern)
  name.pattern <- gsub("%ID%",
                       reactID,
                       name.pattern)
  name.pattern <- gsub("%TUBE%",
                       tube,
                       name.pattern)
  name.pattern <- gsub("%TARGET%",
                       target,
                       name.pattern)
  name.pattern <- gsub("%TYPE%",
                       type,
                       name.pattern)
  return(name.pattern)
}

# Main function
RDML <- function(rdmlfile,
                           name.pattern = "%NAME%__%TUBE%",
                           flat.table = FALSE,
                           omit.ntp = TRUE)
{ 
  rdmldoc <- getRDMLdoc(rdmlfile)
  publisher <- getPublisher(rdmldoc)
#   dilutions <- ifelse(publisher == "Roche Diagnostics",
#                       getDilutionsRoche(rdmlfile),
#                       getDilutions(rdmldoc))
  if(publisher == "Roche Diagnostics") {
    dilutions <- getDilutionsRoche(rdmlfile)
  }
  else {
    dilutions <- getDilutions(rdmldoc)
  }
    
  types <- getTypes(rdmldoc)
  targets <- getTargets(rdmldoc)
  plateDims <- getPlateDimensions(rdmldoc)  
  if (publisher == "Roche Diagnostics") sdescs <- getDescriptions(rdmldoc)
  
  nodes <- getNodeSet(
    rdmldoc,
    "//rdml:react",
    namespaces=c(rdml = "http://www.rdml.org"))
  #   Adps<-ifelse(flat.table == TRUE,
  #                yes = array(),
  #                no = list())
  
  if(flat.table) Adps <- array()
  else Adps <- list()
  if(flat.table) Mdps <- array()
  else Mdps <- list()
  
  for(node in nodes) {
    
    reactID <- xmlGetAttr(node, name = "id")
    sampleID <- xmlGetAttr(node[["sample"]], name = "id")
    tubeName <- ifelse(publisher == "Roche Diagnostics",
                       sdescs[sampleID],
                       sampleID)    
    type <- types[sampleID]
    # omit empty Bio-Rad data
    try(
      # omit Roche cleared wells 'ntp' type
    if (!(type == "ntp" && omit.ntp)) {
      for(fdata in node["data", all = TRUE])
      {
        targetID <- xmlGetAttr(fdata[["tar"]], name = "id")
        if(targetID == "") targetID <- "NA"
        sampleName <- generateSampleName(name.pattern,
                                         plateDims,
                                         reactID,
                                         tubeName,
                                         targetID,
                                         type,
                                         publisher)
        
        adps <- sapply(fdata["adp", all = TRUE],
                       function(x) as.numeric(xmlValue(x[["fluor"]])))
        mdps <- sapply(fdata["mdp", all = TRUE],
                       function(x) as.numeric(xmlValue(x[["fluor"]])))
        # flat.table == TRUE
        if(flat.table) {
          # add qPCR data
          if(length(adps) != 0) {
            Adps <- cbind(
              Adps, adps)          
            colnames(Adps) <- 
              c(colnames(Adps)[-length(colnames(Adps))],
                sampleName)
            # works only after first column added
            if(typeof(Adps) == "double")
            {
              Cycles <- sapply(fdata["adp", all = TRUE],
                               function(x) xmlValue(x[["cyc"]]))
              Adps <- as.data.frame(Adps,
                                    row.names = Cycles)
              Cycles <- as.numeric(Cycles)
              Adps <- cbind(Cycles, Adps)
              Adps <- Adps[-2]
            }
          }
          # add melting data
          if(length(mdps) != 0) {
            Mdps <- cbind(
              Mdps , mdps)
            colnames(Mdps) <- 
              c(colnames(Mdps)[-length(colnames(Mdps))],
                sampleName)
            # works only after first column added
            if(typeof(Mdps) == "double")
            {              
              Tmps <- sapply(fdata["mdp", all = TRUE],
                             function(x) xmlValue(x[["tmp"]]))
              Mdps <- 
                as.data.frame(Mdps,
                              row.names = Tmps)
              Tmps <- as.numeric(Tmps)
              Mdps <- cbind(Tmps, Mdps)
              Mdps <- Mdps[-2]
            }
          }
        }
        # flat.table == FALSE
        else {
          # add qPCR data
          if(length(adps) != 0) {
            Adps[[targetID]][[type]] <- cbind(
              Adps[[targetID]][[type]], adps)          
            colnames(Adps[[targetID]][[type]]) <- 
              c(colnames(Adps[[targetID]][[type]])[-length(colnames(Adps[[targetID]][[type]]))],
                sampleName)
            # works only after first column added
            if(typeof(Adps[[targetID]][[type]]) == "double")
            {
              Cycles <- sapply(fdata["adp", all = TRUE],
                               function(x) xmlValue(x[["cyc"]]))
              Adps[[targetID]][[type]] <- 
                as.data.frame(Adps[[targetID]][[type]],
                              row.names = Cycles)
              Cycles <- as.numeric(Cycles)
              Adps[[targetID]][[type]] <- cbind(Cycles, Adps[[targetID]][[type]])
            }
          }
          # add melting data
          if(length(mdps) != 0) {
            Mdps[[targetID]][[type]] <- cbind(
              Mdps[[targetID]][[type]], mdps)
            colnames(Mdps[[targetID]][[type]]) <- 
              c(colnames(Mdps[[targetID]][[type]])[-length(colnames(Mdps[[targetID]][[type]]))],
                sampleName)
            # works only after first column added
            if(typeof(Mdps[[targetID]][[type]]) == "double")
            {
              Tmps <- sapply(fdata["mdp", all = TRUE],
                             function(x) xmlValue(x[["tmp"]]))
              Mdps[[targetID]][[type]] <- 
                as.data.frame(Mdps[[targetID]][[type]],
                              row.names = Tmps)
              Tmps <- as.numeric(Tmps)
              Mdps[[targetID]][[type]] <- cbind(Tmps, Mdps[[targetID]][[type]])
            }
          }
        }
      }
    }
    , silent = TRUE)
  }
  output <- list(
    Dilutions = dilutions,
    qPCR = Adps,
    Melt = Mdps)
  # remove empty elements (Dilutions, Adps or Mdps)
  output[sapply(output, function(x) length(x) == 0)] <- NULL
  return(output)  
}