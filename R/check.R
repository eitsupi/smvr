#' Check if an object is a `smvr` object
#' @param x An object.
#' @return Indicates whether `x` is a [smvr] object.
#' @examples
#' is_smvr(smvr(1, 2, 3))
#' @export
is_smvr <- function(x) {
  inherits(x, "smvr")
}

#' Check if the `smvr` object has a specific component
#'
#' @description
#' These functions check if the [smvr] object has a specific component.
#'
#' - `is_pre_release()`: Checks if the pre-release identifiers are present.
#' - `has_build_metadata()`: Checks if the build metadata is present.
#' @param x A [smvr] object.
#' @return Indicates whether `x` has the specified component.
#' @seealso
#' - [extract-component] functions for extracting components from a [smvr] object.
#' @examples
#' v <- parse_semver(c(
#'   "1.0.0", "2.0.0-alpha", "2.0.0-beta", "2.0.0-beta.2+build.123"
#' ))
#' v
#'
#' is_pre_release(v)
#' has_build_metadata(v)
#' @name check-component
NULL

#' @rdname check-component
#' @export
is_pre_release <- function(x) {
  if (!is_smvr(x)) {
    cli::cli_abort(
      "{.code is_pre_release()} only works with {.code smvr} objects."
    )
  }
  !(field(x, "pre_release") |> field("is_empty"))
}

#' @rdname check-component
#' @export
has_build_metadata <- function(x) {
  if (!is_smvr(x)) {
    cli::cli_abort(
      "{.code has_build_metadata()} only works with {.code smvr} objects."
    )
  }
  nzchar(field(x, "build"), keepNA = TRUE)
}
