#' @export
is_smvr <- function(x) {
  inherits(x, "smvr")
}

#' @export
is_pre_release <- function(x) {
  if (!is_smvr(x)) {
    cli::cli_abort(
      "{.code is_pre_release()} only works with {.code smvr} objects."
    )
  }
  !(field(x, "pre_release") |> field("is_empty"))
}

#' @export
has_build_metadata <- function(x) {
  if (!is_smvr(x)) {
    cli::cli_abort(
      "{.code has_build_metadata()} only works with {.code smvr} objects."
    )
  }
  nzchar(field(x, "build"), keepNA = TRUE)
}
