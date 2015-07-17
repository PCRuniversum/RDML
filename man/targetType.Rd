% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/RDML.types.R
\docType{class}
\name{targetType}
\alias{targetType}
\title{targetType R6 class.}
\format{An \code{\link{R6Class}} generator object.}
\usage{
targetType
}
\description{
A target is a defined PCR reaction. PCR reactions for the same gene which
differ in primer sequences are considered different targets.\cr Inherits:
\link{rdmlBaseType}.
}
\section{Initialization}{
 \code{targetType$new(id, description = NULL,
  documentation = NULL, xRef = NULL, type, amplificationEfficiencyMethod =
  NULL, amplificationEfficiency = NULL, amplificationEfficiencySE = NULL,
  detectionLimit = NULL, dyeId, sequences = NULL, commercialAssay = NULL)}
}

\section{Fields}{
 \describe{ \item{\code{id}}{\link{idType}.}
  \item{\code{description}}{\link[assertthat]{is.string}.}
  \item{\code{documentation}}{\code{list} of \link{idReferencesType}.}
  \item{\code{xRef}}{\code{list} of \link{xRefType}.}
  \item{\code{type}}{\link{targetTypeType}.}
  \item{\code{amplificationEfficiencyMethod}}{\link[assertthat]{is.string}.}
  \item{\code{amplificationEfficiency}}{\link[base]{double}.}
  \item{\code{amplificationEfficiencySE}}{\link[base]{double}.}
  \item{\code{detectionLimit}}{\link[base]{double}.}
  \item{\code{dyeId}}{\link{idReferencesType}.}
  \item{\code{sequences}}{\link{sequencesType}.}
  \item{\code{commercialAssay}}{\link{commercialAssayType}.} }
}
\keyword{datasets}
