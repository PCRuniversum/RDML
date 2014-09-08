\name{summary-RDML_object}
\alias{summary.RDML_object}
\alias{summary,RDML_object-method}

\title{
Summary RDML_objects
}
\description{
Print and silently returns summary statistics of \code{RDML_object}s.
}
\usage{
\method{summary}{RDML_object}(object, print = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
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
A list of length 2:
\item{dil_table}{A list of dilutions used in experiments.}
\item{exp_tab}{Summary statistics describing experiments.}
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
}

\keyword{ manip }
\keyword{ summary }

