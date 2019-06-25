## ---- echo=FALSE, message=FALSE------------------------------------------
knitr::opts_chunk$set(collapse=T, comment="#>")

## ---- results="hide"-----------------------------------------------------
# Load the RDML package and use its functions to `extract` the required data
library(RDML)
filename <- system.file("extdata/stepone_std.rdml", package="RDML")
raw_data <- RDML$new(filename=filename)

## ------------------------------------------------------------------------
raw_data$target

## ------------------------------------------------------------------------
raw_data$experiment[["Standard Curve Example"]]$run

## ------------------------------------------------------------------------
raw_data_tab <- raw_data$AsTable(
    # Custom name pattern 'position~sample~sample.type~target~run.id'
    name.pattern=paste(
        react$position,
        react$sample$id,
        private$.sample[[react$sample$id]]$type$value,
        data$tar$id,
        run$id$id, # run id added to names
        sep="~"))
# Get all fluorescence data and assign them to the object fdata
fdata <- as.data.frame(raw_data$GetFData(raw_data_tab, long.table=FALSE))

## ------------------------------------------------------------------------
# Load the ggplot2 package for plotting
library(ggplot2)
# Load the reshape2 package to rearrange the data
library(reshape2)
# Rearrange and plot the raw data
fdata_gg <- melt(fdata, id.vars="cyc")
ggplot(data=fdata_gg, aes(x=cyc, y=value, color=variable)) + 
    geom_line() + labs(x="Cycle", y="RFU") + theme_light() +
    theme(legend.position="top",
    legend.direction="horizontal")


## ---- echo=TRUE, include=TRUE--------------------------------------------
# Use the magrittr package to create pipes
library(magrittr)

# Write a custom function that calculates the Cq values and other curve parameters
library(qpcR)
res_fit <- do.call(cbind, lapply(2L:ncol(fdata), function(block_column) {
    
    res_model <- try(mselect(pcrfit(data=fdata, cyc=1, fluo=block_column), verbose=FALSE, do.all=TRUE), silent=TRUE)
    if(res_model %>% class=="try-error") {
        res_model <- NA
    }
    else{
        try(efficiency(res_model, plot=FALSE, type="Cy0"), silent=TRUE)
    }
        }
    )
)
# Assign column names
colnames(res_fit) <- colnames(fdata)[-1]

# Fetch only the Cq values (second derivative maximum) and combine them in a
# data.frame

Cq_SDM <- res_fit[rownames(res_fit)=="cpD2", ] %>% unlist %>% as.data.frame
colnames(Cq_SDM) <- c("Cq")

# Prepare the dilutions and calculated Cq values for further usage in the effcalc
# function from the chipPCR package

dilution <- c(as.factor("ntc"), as.factor("unk"), 10000, 5000, 2500, 1250, 625)
Cq_values <- matrix(Cq_SDM[, "Cq"], nrow=length(dilution), ncol=3, byrow=TRUE)

## ---- results="asis", echo=FALSE-----------------------------------------
knitr::kable(res_fit[, c(1, 5)], caption="Selected data of calculated values 
from the fitted models. The values reported in the table were calculated by a 
chain of functions from the **qpcR** package. In detail, this were the *mselect* 
function to calculate the optimal multiparametric sigmoid model and the 
*efficiency* function to calculate the Cq values and other curve parameters. 
More information about these functions is available from the manual of the 
**qpcR** package. The samples `A01` (ntc, non-template cntrol) and 
`A05` (unk, unknown) were arbitrarily selected for 
presentation. The custom made function assigned a NA (not available) to sample 
`A01` because no Cq value or other curve 
parameters could be calculated from the non-template control.")

## ---- echo=TRUE----------------------------------------------------------
# Load the chipPCR package
library(chipPCR)

# Use the effcalc function from the chipPCR package to calculate the amplification
# efficiency and store the results in the object res_efficiency

res_efficiency <- effcalc(dilution[-c(1:3)], Cq_values[-c(1:3), ], logx=TRUE)

# Use the %>% function from the magrittr package to plot the results (res_efficiency) 
# from the effcalc function 

res_efficiency %>% plot(., CI=TRUE, ylab="Cq (SDM)", 
                        main="Second Derivative Maximum Method")

## ---- results="asis"-----------------------------------------------------
# Combine the sample labels and the Cq values as calculate by the Second 
# Derivative Maximum Method (cpD2).

sample_Cq <- data.frame(sample=c("ntc", "unk", 
                                 10000, 5000, 2500, 1250, 625), 
                        Cq_values)

# Print table of all Cq values
# Use the kable function from the knitr package to print a table

knitr::kable(sample_Cq, caption="Cq values as calculate by the Second Derivative 
Maximum Method (cpD2). ntc, non-template control. unk, unknown sample. X1, X2 
and X3 are the Cq values from a triplicate measurement.")

## ---- results="asis"-----------------------------------------------------
knitr::kable(res_efficiency, caption="Analysis of the amplification efficiency. 
The table reports the concentration-depentent average Cq values from three 
replicates per dilution step. In addition, the standard deviation (SD) and the 
Coefficient of Variation (RSD [%]) are presented. The results indicate that the 
data basis for the calibration curve is valid.")

