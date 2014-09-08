\name{summary-RDML_object}
\alias{summary.RDML_object}
\alias{summary,RDML_object-method}

\title{
Summary RDML_objects
}
\description{
Prints and silently returns summary statistics of \code{RDML_object}s.
}
\usage{
\method{summary}{RDML_object}(object, print = TRUE, ...)
}

\arguments{
  \item{object}{
an object of class \code{RDML_object}.
}
  \item{print}{
logical, if \code{TRUE} summary is printed to console.
}
  \item{...}{
currently ignored.
}
}


\value{
A list of length at most 3 (less if \code{object} does not contain information about 
dilutions, qPCR or melting data):
\item{dil_table}{A list of dilutions used in experiments.}
\item{melt_table}{Melting data.}
\item{exp_tab}{Summary statistics describing experiments: mean, median, standard deviation,
median absolute deviation, interquartile range, medcouple, skewness, SNR, VRM, number of NAs,
intercept, slope, R squared, Breusch-Pagan test p-value.}
}

\author{
Michal Burdukiewicz, Konstantin A. Blagodatskikh, Stefan Roediger
}

\seealso{
Summary statistics are calculated using \code{\link[chipPCR]{MFIaggr}}.
}
\examples{
lc96 <- RDML(system.file("extdata/lc96_bACTXY.rdml", package = "RDML"))
summary(lc96)
#store raw results of summary without printing
res <- summary(lc96, print = FALSE)
}

\keyword{ manip }
\keyword{ summary }

