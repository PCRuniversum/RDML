## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")

## ---- message = FALSE----------------------------------------------------
library(RDML)
library(chipPCR)
library(MBmca)
library(ggplot2)
data(C54)

## ------------------------------------------------------------------------
str(C54)

## ----warning=FALSE, fig.width = 6, fig.height = 4------------------------

dat <- data.frame(Cycle = rep(C54[, 1], ncol(C54) - 1),
                  Experiment = unlist(lapply(colnames(C54)[-1], function(i) rep(i, nrow(C54)))),
                  refMFi = unlist(C54[, -1]))
levels(dat[["Experiment"]]) <- c("D1 - Stock cDNA", "D2 - 1/10 cDNA", "D3 - 1/100 cDNA")

ggplot(dat, aes(x = Cycle, y = refMFi, colour = Experiment)) + geom_point()

## ------------------------------------------------------------------------
RDML$set("public", "ProcessVideoScan",
         function(last.cycle,
                  bg.range) {
           # Get curves description
           tab <- as.data.frame(self$AsTable())
           # Get fluorescent point
           dat <- as.data.frame(self$GetFData(tab))
           # Give new names to runs
           # Preprocess amplification curve raw data as described in 
           # R&ouml;diger et al. (2015) Bioinformatics. Note that the dataset
           # has different length of cycle numbers and missing values (NAs).
           # The CPP function from the chipPCR package prepares the data for 
           # further analysis.
           tab[, "run.id"] <- paste0(tab[, "run.id"], "_CPP")
           
           # Curves will be used one by one
           for(i in 2:length(dat)) {
             # Preprocess data
             preprocessed <- CPP(dat[1:last.cycle[i - 1], 1], 
                                 dat[1:last.cycle[i - 1], i],
                                 trans = TRUE,
                                 bg.range = bg.range[[i - 1]])[["y.norm"]]
             # Create data.frame with cycle number and preprocessed curve Then
             # give name to fluorescence points columns as before preprocessing
             dat_CPP <- cbind(dat[1:last.cycle[i - 1], 1],
                              preprocessed)
             colnames(dat_CPP)[2] <- tab$fdata.name[i - 1]
             # Set preprocessed data with new description (new run names)
             self$SetFData(dat_CPP, tab)
             # Calculate and set Cq
             # Set Cq from second derivative maximum method as described in 
             # R&ouml;diger et al. (2015) Bioinformatics for preprocessed data.
             # The diffQ2 function from the MBmca package 
             # (R&ouml;diger et al. (2013), The R Journal) was used to calculate the
             # Cq values of each amplification curve.
             cq <- diffQ2(dat_CPP, inder = TRUE)[["xTm1.2.D2"]][1]
             self$experiment[[tab[i - 1, "exp.id"]]]$
               run[[tab[i - 1, "run.id"]]]$
               react[[tab[i - 1, "react.id"]]]$
               data[[tab[i - 1, "target"]]]$cq <- cq
           }
         }, overwrite = TRUE
)

## ------------------------------------------------------------------------
# Create a data frame of metadata 
description <- data.frame(
  fdata.name = c("D1", "D2", "D3"),
  exp.id = c("exp1", "exp1", "exp1"),
  run.id = c("run1", "run2", "run3"),
  react.id = c(1, 1, 1),
  sample = c("D1 - Stock cDNA", "D2 - 1/10 cDNA", "D3 - 1/100 cDNA"),
  sample.type = c("unkn", "unkn", "unkn"),
  target = c("MLC-2v", "MLC-2v", "MLC-2v"),
  target.dyeId = c("Cy5", "Cy5", "Cy5"),
  stringsAsFactors = FALSE
)

# Create an empty RDML object
video.scan <- RDML$new()

# Add fluorescence data and metadata to the RDML object from a given source.
# For the sake of simplicity we use the C54 dataset from the chipPCR package.
video.scan$SetFData(C54, description)

## ------------------------------------------------------------------------
# Add experimentalist information
video.scan$experimenter <- 
  list(
    experimenterType$new(
      idType$new("SR"),
      "Stefan",
      "Roediger",
      "stefan.roediger@b-tu.de"
    ),
    experimenterType$new(
      idType$new("CD"),
      "Claudia",
      "Deutschmann"
    )
  )

# Add a reference to documentation
video.scan$documentation <- list(
  documentationType$new(
    idType$new("Roediger et al. 2013"),
    paste("A Highly Versatile Microscope Imaging Technology Platform for the Multiplex",
          "Real-Time Detection of Biomolecules and Autoimmune Antibodies. S. Roediger,",
          "P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C. Schmidt, M.",
          "Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C. Schroeder. Advances in",
          "Biochemical Bioengineering/Biotechnology. 133:33-74, 2013.",
          "https://www.ncbi.nlm.nih.gov/pubmed/22437246")
  )
)

cdna <- 
  cdnaSynthesisMethodType$new(
    enzyme = "SuperScript II",
    primingMethod =
      primingMethodType$new("oligo-dt"),
    dnaseTreatment = TRUE
  )
video.scan$sample$`D1 - Stock cDNA`$description <- "Input stock cDNA was used undiluted (D1)"
video.scan$sample$`D1 - Stock cDNA`$cdnaSynthesisMethod <- cdna
video.scan$sample$`D2 - 1/10 cDNA`$description <- "1/1000 diluted in A. bidest"
video.scan$sample$`D2 - 1/10 cDNA`$cdnaSynthesisMethod <- cdna
video.scan$sample$`D3 - 1/100 cDNA`$description <- "1/1000000 diluted in A. bidest"
video.scan$sample$`D3 - 1/100 cDNA`$cdnaSynthesisMethod <- cdna

video.scan$target$`MLC-2v`$xRef <- list(
  xRefType$new("uniprot",
               "P10916")
)
video.scan$target$`MLC-2v`$sequences <- 
  sequencesType$new(
    forwardPrimer <- oligoType$new(
      sequence = "ACAGGGATGGCTTCATTGAC"),
    reversePrimer <- oligoType$new(
      sequence = "ATGCGTTGAGAATGGTTTCC"),
    probe1 <- oligoType$new(
      threePrimeTag = "Atto647N",
      sequence = "CAGGGTCCGCTCCCTTAAGTTTCTCC",
      fivePrimeTag = "BHQ2")
  )

tcc <- 
  thermalCyclingConditionsType$new(
    idType$new("Amplification"),
    experimenter = list(
      idReferencesType$new("SR"),
      idReferencesType$new("CD")
    ),
    step = 
      list(
        stepType$new(
          nr = 1,
          temperature = temperatureType$new(95,
                                            600)
        ),
        stepType$new(
          nr = 2,
          temperature = temperatureType$new(95,
                                            40)
        ),
        stepType$new(
          nr = 3,
          temperature = temperatureType$new(58.5,
                                            90)
        ),
        stepType$new(
          nr = 4,
          temperature = temperatureType$new(68.5,
                                            90)
        ),
        stepType$new(
          nr = 5,
          loop = loopType$new(goto = 2,
                              repeat.n = 49)
        )
      )
  )
video.scan$thermalCyclingConditions <- list(
  tcc
)

#add description of the experiment
video.scan$experiment$exp1$description <- 
  paste("The aim was to amplify MLC-2v in the VideoScan and to monitor with a",
        "hydrolysis probe for MLC-2v. The primer sequences for MLC-2v were taken",
        "from Roediger et al. (2013). The amplification was detected in solution of",
        "the 1 HCU (see Roediger et al. 2013 for details). A 20 micro L PCR reaction",
        "was composed of 250 nM primer (forward and reverse), 1x Maxima Probe qPCR",
        "Master Mix (Fermentas), 1 micro L template (MLC-2v amplification product in",
        "different dilutions), 50 nM hydrolysis probe for MLC-2v and A.",
        "bidest. During the amplification, fluorescence was measured at 59.5 degree",
        "Celsius. The Cy5 channel was used to monitor the MLC-2v specific hydrolysis",
        "probe. Input stock cDNA was used undiluted (D1). D2 was 1/1000 and D3",
        "1/1000000 diluted in A. bidest. The D1, D2, and D3 have different numbers",
        "measure points and D2 contains a missing value at cycle 37.")
video.scan$experiment$exp1$run$run1$thermalCyclingConditions <- idReferencesType$new("Amplification")
video.scan$experiment$exp1$run$run2$thermalCyclingConditions <- idReferencesType$new("Amplification")
video.scan$experiment$exp1$run$run3$thermalCyclingConditions <- idReferencesType$new("Amplification")

## ---- message = FALSE----------------------------------------------------
# Process VideoScan data
video.scan$ProcessVideoScan(last.cycle = c(35, 45, 55),
                            bg.range = list(c(1,8),
                                            NULL,
                                            NULL))

## ---- results = "hide", fig.width = 6, fig.height = 4--------------------
# Visualize RDML object
video.scan$AsDendrogram()

## ---- results = "hide", fig.width = 6, fig.height = 4--------------------
## Visualise preprocessed data with Cq values as vertical dashed lines
# Add custom column that contains the calculated Cq
tab <- video.scan$AsTable(
  cq = {
    cq <- data$cq
    if (is.null(cq) || is.na(cq))
      NULL
    else
      cq
  })
# Get preprocessed data in 'long.table' format
dat <- video.scan$GetFData(tab[grepl("_CPP", tab[["run.id"]]), ],
                           long.table = TRUE)
ggplot(dat, aes(x = cyc, y = fluor)) +
  geom_line(aes(group = fdata.name, color = fdata.name),
            size = 1.2) +
  geom_vline(aes(xintercept = cq, color = fdata.name),
             linetype = "longdash") +
  scale_x_continuous("Cycle") +
  scale_y_continuous("Fluorescence") +
  ggtitle("Amplification of human MLC-2v\nVideoScan HCU")

