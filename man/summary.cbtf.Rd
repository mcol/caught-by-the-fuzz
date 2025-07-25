% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary.cbtf}
\alias{summary.cbtf}
\title{Results summary from a fuzz run}
\usage{
\method{summary}{cbtf}(object, ...)
}
\arguments{
\item{object}{An object of class \code{cbtf}.}

\item{...}{Further arguments passed to or from other methods.
These are currently ignored.}
}
\value{
A data frame with the following columns is returned invisibly:
\item{fun}{The names of the function tested.}
\item{what}{The inputs tested.}
\item{res}{One of "OK", "FAIL", "WARN" or "SKIP" for each function
tested.}
\item{msg}{The message received in case of error, warning or skip,
or an empty string if the function succeeded.}
}
\description{
Generates a data frame of summary results from a single run of \code{\link[=fuzz]{fuzz()}}.
}
