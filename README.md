![RDML](https://github.com/kablag/RDML/blob/master/inst/RDML_logo.png)


Imports qPCR data from RDML v1.1 format files (Lefever et al. 2009) and 
transforms it to the appropriate format of the qpcR package (Ritz et al. 
2008, Spiess et al. 2008) or chipPCR package. RDML is the recommended file 
format element in the Minimum Information for Publication of Quantitative 
Real-Time PCR Experiments (MIQE) guidelines (Bustin et al. 2009). Data can be 
imported to the one table format (flat) or list of tables spited by PCR targets 
and samples types. Tables consist of 'Cycles' in the first column and 
fluorescence data of the samples in the remaining columns for qPCR data. For 
melting data tables consist of 'Temperature' in the first column and 
fluorescence data of the samples in the remaining columns.

Installation
------------

You can install the latest development version of the code using the `devtools` R package.

```
# Install devtools, if you haven't already.
install.packages("devtools")

library(devtools)
install_github("RDML", "kablag")
```
