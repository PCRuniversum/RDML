% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/RDML.types.R
\docType{class}
\name{experimentType}
\alias{experimentType}
\title{experimentType R6 class.}
\format{An \code{\link{R6Class}} generator object.}
\usage{
experimentType
}
\description{
An experiment can contain several runs (\link{runType}).\cr Inherits:
\link{rdmlBaseType}.
}
\section{Initialization}{
 \code{experimentType$new(id, description = NULL,
  documentation = NULL, run = NULL)}
}

\section{Fields}{
 \describe{
\item{\code{id}}{\link{idType}.}
\item{\code{description}}{\link[assertthat]{is.string}.}
\item{\code{documentation}}{\code{list} of \link{idReferencesType}.}
\item{\code{run}}{\code{list} of \link{runType}.}
}
}

\section{Methods}{
 \describe{\item{\code{AsDataFrame(dp.type = "adp",
  long.table = FALSE)}}{Represents amplification (\code{dp.type = "adp"}) or
  melting (\code{dp.type = "mdp"}) data points as \code{data.frame}.
  \code{long.table = TRUE} means that fluorescence data for all runs and
  reacts will be at one collumn.}}
}
\keyword{datasets}
