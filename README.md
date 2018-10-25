[![published in: Bioinformatics](https://img.shields.io/badge/published%20in-Bioinformatics-ff69b4.svg?style=flat)](https://doi.org/10.1093/bioinformatics/btx528)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RDML)](https://CRAN.R-project.org/package=RDML)
[![Downloads](https://cranlogs.r-pkg.org/badges/RDML)]( https://CRAN.R-project.org/package=RDML)
[![Travis-CI Build Status](https://travis-ci.org/kablag/RDML.svg?branch=master)](https://travis-ci.org/kablag/RDML)
[![Rdoc](http://www.rdocumentation.org/badges/version/RDML)](http://www.rdocumentation.org/packages/RDML) 

<img src="https://raw.githubusercontent.com/kablag/RDML/master/vignettes/RDML_logo.png" alt="RDML" width="100%">

The RDML package is published in Oxford Bioinformatics: 
Stefan Rödiger, Michał Burdukiewicz, Andrej-Nikolai Spiess, Konstantin Blagodatskikh; Enabling reproducible real-time quantitative PCR research: the RDML package, Bioinformatics, [https://doi.org/10.1093/bioinformatics/btx528](https://doi.org/10.1093/bioinformatics/btx528) (see also `citation()`).

Imports qPCR data from RDML v1.1 format files ([Lefever et al. 2009](http://nar.oxfordjournals.org/content/32/5/1792.abstract?view=long&pmid=19223324)) and 
transforms it to the appropriate format of the qpcR package (Ritz et al. 2008, 
Spiess et al. 2008) or chipPCR package. [RDML](http://www.rdml.org/) (Real-time PCR Data Markup 
Language) is the recommended file format element in the Minimum Information for 
Publication of Quantitative Real-Time PCR Experiments (MIQE) guidelines ([Bustin et al. 2009](http://clinchem.aaccjnls.org/content/55/4/611.long)).

# Installation
------------

The stable version of the `RDML` package for R is hosted on [CRAN](https://CRAN.R-project.org/package=RDML) and can be installed as any R package.

You can install the latest development version of the code using the `devtools` R package.

```r
# Install devtools, if you haven't already.
install.packages("devtools")

library(devtools)
install_github("kablag/RDML")
```

# Manual

The manual is available [online](https://kablag.github.io/RDML/).

# Examples

`RDML` imports various data formats (CSV, XMLX) besides the RDML format. Provided that the raw data 
have a defined structure (as described in the vignette) the import should be 
done by a few clicks. The example below shows the import of amplification curve
data, which were stored in a CSV file. The function `rdmlEdit()` was used in the 
[RKWard IDE/GUI](https://rkward.kde.org/) for further processing. rdmlEdit may be also accessed as a web server (http://shtest.evrogen.net/rdmlEdit/). 

<img src="https://raw.githubusercontent.com/kablag/RDML/master/vignettes/File_import.png" alt="Data Import" width="100%">

Once imported enables `rdmlEdit()` and other functions from the `RDML` package complex 
data visualization and processing in the R statistical computing environment.

<img src="https://raw.githubusercontent.com/kablag/RDML/master/vignettes/data_view.png" alt="Data View" width="100%">

