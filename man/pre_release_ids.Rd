% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pre_release_ids.R, R/parse.R
\name{new_pre_release_ids}
\alias{new_pre_release_ids}
\alias{parse_pre_release_ids}
\alias{pre_release_ids}
\title{Pre-release identifiers}
\usage{
new_pre_release_ids(...)

parse_pre_release_ids(x)
}
\arguments{
\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}>
Single pre-release identifiers.
Each identifier can be something to be cast to a \link{pre_release_identifier}
vector by \code{\link[vctrs:vec_cast]{vctrs::vec_cast()}}.
All components must be of the same length or length 1 (will be recycled).}

\item{x}{A character vector representing pre-release identifiers.
Each identifier separated by a dot (\code{.}) will be parsed as a
\link{pre_release_identifier}.}
}
\value{
A \link{pre_release_ids} vector.
}
\description{
A class representing a collection of \link[=pre_release_identifier]{identifiers},
which are used for representing pre-release versions.

There are two functions to create the \link{pre_release_ids} vector:
\itemize{
\item \code{\link[=pre_release_ids]{pre_release_ids()}} is a low-level constructor for creating
pre-release identifiers from individual components.
\item \code{\link[=parse_pre_release_ids]{parse_pre_release_ids()}} parses a character vector into
pre-release identifiers.
}
}
\details{
If the components are empty, they are treated as the highest precedence
pre-release ids, which is used to indicate that the version is \emph{not
a pre-release version}.
}
\section{Limitations}{

There are some limitations on the number of identifiers
in some operations:
\itemize{
\item When comparing with a string, the number of identifiers in the string.
If it exceeds 5, an error is raised.
\item When assigning, the number of identifiers in the value being assigned.
If it exceeds the number of identifiers in the target or 5,
whichever is larger, an error is raised.
}

Please refer to the examples for details.
}

\examples{
# Each components are concatenated with a dot
new_pre_release_ids("rc", 1:3)

ids <- parse_pre_release_ids(
  c("", "alpha.beta", "alpha.1", "beta", "beta.11", "beta.2")
)
ids

# Empty ids have the highest precedence
# (Used to indicate not a pre-release version)
vctrs::vec_sort(ids)

# Can be compared with string notation
ids[ids > "beta.2"]

# Limitations:
# 1. When comparing with a string, the number of identifiers in the string
#    must not exceed 5.
try(ids[ids > "beta.2.3.4.5.6"])

# This works since the string is parsed first.
ids[ids > parse_pre_release_ids("beta.2.3.4.5.6")]

# 2. When assigning, the number of identifiers in the value being assigned
#    must not exceed the number of identifiers in the target or 5,
#    whichever is larger.
try(ids[1] <- parse_pre_release_ids("beta.2.3.4.5.6"))
}
