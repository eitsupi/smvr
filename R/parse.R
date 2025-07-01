#' @param x A character vector representing semantic versions.
#'   Each version should follow the
#'   [Semantic Versioning Specification](https://semver.org/).
#'   Partial matches are not allowed (e.g., `"1.0"` is not valid).
#' @rdname smvr
#' @order 2
#' @export
parse_semver <- function(x) {
  x <- vec_cast(x, character(), call = caller_env())

  pattern <- paste0(
    "^",
    "(0|[1-9][0-9]*)\\.", # major
    "(0|[1-9][0-9]*)\\.", # minor
    "(0|[1-9][0-9]*)", # patch
    "(?:-([0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*))?", # pre-release
    "(?:\\+([0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*))?", # build
    "$"
  )
  parts <- regmatches(x, regexec(pattern, x))

  invalid <- map_lgl(parts, \(x) length(x) == 0L) & !is.na(x)

  if (any(invalid)) {
    cli::cli_warn(c(
      `!` = "Invalid version strings detected, setting to {.code NA}.",
      x = "Problematic values: {.val {x[invalid]}}"
    ))
  }

  major <- map_chr(parts, \(x) x[2]) |>
    as.integer()
  minor <- map_chr(parts, \(x) x[3]) |>
    as.integer()
  patch <- map_chr(parts, \(x) x[4]) |>
    as.integer()
  pre_release <- map_chr(parts, \(x) x[5]) |>
    parse_pre_release_ids_impl(
      call = caller_env(),
      check = FALSE
    )
  build <- map_chr(parts, \(x) x[6])

  smvr(
    major = major,
    minor = minor,
    patch = patch,
    pre_release = pre_release,
    build = build
  )
}

#' @param x A character vector representing pre-release identifiers.
#'   Each identifier separated by a dot (`.`) will be parsed as a
#'   [pre_release_identifier].
#' @rdname pre_release_ids
#' @order 2
#' @export
parse_pre_release_ids <- function(x) {
  parse_pre_release_ids_impl(x, call = caller_env())
}

#' Internal implementation of parsing function for pre-release ids
#'
#' This is an internal function that parses pre-release identifiers
#' from a character vector. Only the difference to [parse_pre_release_ids()]
#' is that it allows to skip the validation step.
#' @inherit parse_pre_release_ids return
#' @inheritParams pre_release_ids
#' @inheritParams cli::cli_warn
#' @param ... Ignored.
#' @param check `TRUE` (default) or `FALSE`. If `TRUE`, the function will
#'   check if the pre-release identifiers are valid according to the
#'   regex pattern. If `FALSE`, it will skip the validation step.
#' @keywords internal
parse_pre_release_ids_impl <- function(
  x,
  ...,
  call = caller_env(),
  check = TRUE
) {
  x <- vec_cast(x, character(), call = call)

  if (check) {
    pattern <- "^([0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*)$"
    parts <- regmatches(x, regexec(pattern, x))

    invalid <- map_lgl(parts, \(x) vec_size(x) == 0L) & !is.na(x) & nzchar(x)

    if (any(invalid)) {
      cli::cli_warn(
        c(
          `!` = "Invalid pre-release ids detected, setting to {.code NA}.",
          x = "Problematic values: {.val {x[invalid]}}"
        ),
        call = call
      )
    }

    checked <- map_chr(parts, \(x) x[2])
  } else {
    checked <- x
    # Avoid character(0) after splitting
    checked[!nzchar(x)] <- NA_character_
    invalid <- FALSE
  }

  values <- vec_rbind(
    !!!strsplit(checked, "\\."),
    .name_repair = "unique_quiet"
  )
  # Fill with empty strings to ensure each row has the same length
  values[is.na(values)] <- ""

  out <- new_pre_release_ids(!!!values)

  # Fix NAs
  out[is.na(x) | invalid] <- NA
  out
}
