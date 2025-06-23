#' pre_release_identifier: Single pre-release identifier for Semantic Versioning
#'
#' @description
#' A class representing a single pre-release identifier (string or integer) for Semantic Versioning 2.0.
#' @param x Character or integer value
#' @return An object of class pre_release_identifier
#' @export
new_pre_release_identifier <- function(x = character()) {
  # x should be character
  x <- vec_cast(x, character())

  # Only [0-9a-zA-Z-]* allowed
  invalid <- !is.na(x) & !grepl("^[0-9a-zA-Z-]*$", x)
  if (any(invalid)) {
    cli::cli_abort(
      "Invalid pre-release identifier: only [0-9a-zA-Z-] allowed in each identifier."
    )
  }

  is_empty <- !nzchar(x)
  is_num <- is.na(x) | is_empty | grepl("^[0-9]+$", x) & !grepl("^0[0-9]+", x)

  alphanumeric_id <- x
  numeric_id <- double(length(x))

  # Numeric identifiers always have lower precedence than
  # non-numeric identifiers
  numeric_id[!is_num] <- Inf
  numeric_id[is_num & !is_empty] <- as.numeric(x[is_num & !is_empty])
  numeric_id[is_empty] <- -Inf

  out <- new_rcrd(
    list(
      numeric = numeric_id,
      alphanumeric = alphanumeric_id
    ),
    class = "pre_release_identifier"
  )

  # Fix NAs
  out[!vec_detect_complete(vec_data(out))] <- NA
  out
}

#' @export
format.pre_release_identifier <- function(x, ...) {
  field(x, "alphanumeric")
}

#' @export
vec_ptype2.pre_release_identifier.pre_release_identifier <- function(
  x,
  y,
  ...
) {
  x
}

#' @export
vec_cast.pre_release_identifier.pre_release_identifier <- function(x, to, ...) {
  x
}

#' @export
vec_ptype2.pre_release_identifier.logical <- function(x, y, ...) {
  new_pre_release_identifier()
}
#' @export
vec_ptype2.logical.pre_release_identifier <- function(x, y, ...) {
  new_pre_release_identifier()
}

#' @export
vec_ptype2.pre_release_identifier.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.pre_release_identifier <- function(x, y, ...) character()

#' @export
vec_ptype2.pre_release_identifier.integer <- function(x, y, ...) {
  new_pre_release_identifier()
}
#' @export
vec_ptype2.integer.pre_release_identifier <- function(x, y, ...) {
  new_pre_release_identifier()
}

#' @export
vec_cast.pre_release_identifier.logical <- function(x, to, ...) {
  if (all(is.na(x))) {
    new_pre_release_identifier(rep(NA_character_, length(x)))
  } else {
    cli::cli_abort("Cannot cast non-NA logical to pre_release_identifier")
  }
}

#' @export
vec_cast.character.pre_release_identifier <- function(x, to, ...) {
  field(x, "alphanumeric")
}

#' @export
vec_cast.pre_release_identifier.character <- function(x, to, ...) {
  new_pre_release_identifier(x)
}

#' @export
vec_cast.integer.pre_release_identifier <- function(x, to, ...) {
  vec_data(x) |>
    as.integer() |>
    suppressWarnings()
}

#' @export
vec_cast.pre_release_identifier.integer <- function(x, to, ...) {
  new_pre_release_identifier(as.character(x))
}

#' @export
vec_cast.double.pre_release_identifier <- function(x, to, ...) {
  vec_data(x) |>
    as.numeric() |>
    suppressWarnings()
}

#' @export
vec_cast.pre_release_identifier.double <- function(x, to, ...) {
  new_pre_release_identifier(as.character(x))
}

#' @export
vec_proxy_compare.pre_release_identifier <- function(x, ...) {
  vec_data(x)
}
