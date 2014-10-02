\name{RDML_gui}
\alias{RDML_gui}

\title{
Graphical User Interface for .rdml data import
}
\description{
\code{RDML_gui} is a web-browser based graphical interface allowing user to read 
\code{.rdml} file, extract data and save it in an other format. Additional features 
include summary statistics and plotting functions.
}
\usage{
RDML_gui()
}

\author{
Konstantin A. Blagodatskikh <k.blag@yandex.ru>, 
Stefan Roediger <stefan.roediger@hs-lausitz.de>,
Michal Burdukiewicz <michalburdukiewicz@gmail.com>
}
\section{Warning }{
Any ad-blocking software may cause of malfunctions. 
}

\examples{
\donttest{
library(shiny)
RDML_gui()
}
}

\seealso{
Read \code{.rdml} files: \code{\link{RDML}}.
Select data from \code{.rdml} files: \code{\link{selectFData}}.
Summary statistics: \code{\link{summary.RDML_object}}.
}

\keyword{ hplot }
\keyword{ shiny }
\keyword{ GUI }
\keyword{ browser }