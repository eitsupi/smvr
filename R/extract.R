#' Extract each component of version numbers/labels
#'
#' These functions extract the individual components of
#' version numbers or labels, such as major, minor,
#' patch numbers, or, pre-release identifiers and build metadata.
#' @param x A version object.
#' @param ... Additional arguments passed to methods.
#' @return The extracted component of the version object.
#' - [extract_major()], [extract_minor()], and
#'   [extract_patch()] return integer.
#' - [extract_pre_release_ids()] returns [pre_release_ids].
#' - [extract_build_metadata()] returns character.
#' @seealso
#' - [smvr()] to create a [smvr] object from components.
#' - [check-component] functions for checking if
#'   [smvr] object has a specific component.
#' @examples
#' sem_ver <- parse_semver(c("1.2.3-alpha+001", "2.0.0", NA))
#'
#' extract_major(sem_ver)
#' extract_minor(sem_ver)
#' extract_patch(sem_ver)
#' extract_pre_release_ids(sem_ver)
#' extract_build_metadata(sem_ver)
#'
#' # Extracting version also works for numeric_version
#' num_ver <- numeric_version(c("1", "3.1.4.1.5", NA), strict = FALSE)
#'
#' extract_major(num_ver)
#' extract_minor(num_ver)
#' extract_patch(num_ver)
#' @name extract-component
NULL

#' @rdname extract-component
#' @export
extract_major <- function(x, ...) {
  UseMethod("extract_major")
}

#' @rdname extract-component
#' @export
extract_minor <- function(x, ...) {
  UseMethod("extract_minor")
}

#' @rdname extract-component
#' @export
extract_patch <- function(x, ...) {
  UseMethod("extract_patch")
}

#' @rdname extract-component
#' @export
extract_pre_release_ids <- function(x, ...) {
  UseMethod("extract_pre_release_ids")
}

#' @rdname extract-component
#' @export
extract_build_metadata <- function(x, ...) {
  UseMethod("extract_build_metadata")
}

#' @rdname extract-component
#' @export
extract_major.smvr <- function(x, ...) {
  field(x, "version_core")$major
}

#' @rdname extract-component
#' @export
extract_minor.smvr <- function(x, ...) {
  field(x, "version_core")$minor
}

#' @rdname extract-component
#' @export
extract_patch.smvr <- function(x, ...) {
  field(x, "version_core")$patch
}

#' @rdname extract-component
#' @export
extract_pre_release_ids.smvr <- function(x, ...) {
  field(x, "pre_release")
}

#' @rdname extract-component
#' @export
extract_build_metadata.smvr <- function(x, ...) {
  field(x, "build") |>
    as.character()
}
