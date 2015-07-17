% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/RDML.types.R
\docType{class}
\name{pcrFormatType}
\alias{pcrFormatType}
\title{pcrFormatType R6 class.}
\format{An \code{\link{R6Class}} generator object.}
\usage{
pcrFormatType
}
\description{
The format of the run - This allows the software to display the data
according to the qPCR instrument run format.\cr Inherits:
\link{rdmlBaseType}.
}
\details{
Rotor formats always have 1 column; rows correspond to the number of places
in the rotor. Values for common formats are: \tabular{lllll}{ Format \tab
rows \tab columns \tab rowLabel \tab columnLabel \cr single-well \tab 1
\tab 1       \tab 123      \tab 123         \cr 48-well plate \tab 6    \tab
8       \tab ABC      \tab 123         \cr 96-well plate \tab 8    \tab 12
\tab ABC      \tab 123         \cr 384-well plate \tab 16   \tab 24      \tab
ABC      \tab 123         \cr 1536-well plate \tab 32   \tab 48      \tab ABC
\tab 123         \cr 3072-well array \tab 32   \tab 96      \tab A1a1
\tab A1a1        \cr 5184-well chip \tab 72   \tab 72      \tab ABC      \tab
123         \cr 32-well rotor \tab 32   \tab 1       \tab 123      \tab 123
\cr 72-well rotor \tab 72   \tab 1       \tab 123      \tab 123         \cr
100-well rotor \tab 100  \tab 1       \tab 123      \tab 123         \cr free
format \tab -1   \tab 1       \tab 123      \tab 123 } If rows are -1 then
the software should not try to reconstruct a plate and just display all react
data in list (1 column) form. If columns is 1 then the software should not
display a column label.
}
\section{Initialization}{
 \code{pcrFormatType$new(rows, columns, rowLabel, columnLabel)}
}

\section{Fields}{
 \describe{
\item{\code{rows}}{\link[assertthat]{is.count}.}
\item{\code{columns}}{\link[assertthat]{is.count}.}
\item{\code{rowLabel}}{\link{labelFormatType}.}
\item{\code{columnLabel}}{\link{labelFormatType}.}
  }
}
\keyword{datasets}
