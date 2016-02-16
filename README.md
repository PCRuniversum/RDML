[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RDML)](http://cran.r-project.org/web/packages/RDML)
[![Downloads](http://cranlogs.r-pkg.org/badges/RDML)](http://cran.rstudio.com/package=RDML)

![RDML](https://github.com/kablag/RDML/blob/master/vignettes/RDML_logo.png)


Imports qPCR data from RDML v1.1 format files (Lefever et al. 2009) and 
transforms it to the appropriate format of the qpcR package (Ritz et al. 2008, 
Spiess et al. 2008) or chipPCR package. RDML (Real-time PCR Data Markup 
Language) is the recommended file format element in the Minimum Information for 
Publication of Quantitative Real-Time PCR Experiments (MIQE) guidelines (Bustin 
et al. 2009).

Installation
------------

You can install the latest development version of the code using the `devtools` R package.

```r
# Install devtools, if you haven't already.
install.packages("devtools")

library(devtools)
install_github("kablag/RDML")
```
