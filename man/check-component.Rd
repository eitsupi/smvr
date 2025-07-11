% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{check-component}
\alias{check-component}
\alias{is_pre_release}
\alias{has_build_metadata}
\title{Check if the \code{smvr} object has a specific component}
\usage{
is_pre_release(x)

has_build_metadata(x)
}
\arguments{
\item{x}{A \link{smvr} object.}
}
\value{
Indicates whether \code{x} has the specified component.
}
\description{
These functions check if the \link{smvr} object has a specific component.
\itemize{
\item \code{is_pre_release()}: Checks if the pre-release identifiers are present.
\item \code{has_build_metadata()}: Checks if the build metadata is present.
}
}
\examples{
v <- parse_semver(c(
  "1.0.0", "2.0.0-alpha", "2.0.0-beta", "2.0.0-beta.2+build.123"
))
v

is_pre_release(v)
has_build_metadata(v)
}
\seealso{
\itemize{
\item \link{extract-component} functions for extracting components from a \link{smvr} object.
}
}
