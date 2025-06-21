#' pre_release_identifier: Single pre-release identifier for Semantic Versioning
#'
#' @description
#' A class representing a single pre-release identifier (string or integer) for Semantic Versioning 2.0.
#' @param x Character or integer value
#' @return An object of class pre_release_identifier
#' @export
pre_release_identifier <- function(x = character()) {
  # x should be character
  x <- vec_cast(x, character())

  # Only [0-9a-zA-Z-]* allowed
  invalid <- !is.na(x) & !grepl("^[0-9a-zA-Z-]*$", x)
  if (any(invalid)) {
    cli::cli_abort(
      "Invalid pre-release identifier: only [0-9a-zA-Z-] allowed in each identifier."
    )
  }
  # Determine type: numeric (no leading zero, all digits), else alphanumeric
  is_num <- all(is.na(x) | grepl("^[0-9]*$", x) & !grepl("^0[0-9]+$", x))
  identifier_type <- if (is_num) "numeric" else "alphanumeric"
  vctrs::new_vctr(
    x,
    identifier_type = identifier_type,
    class = "pre_release_identifier"
  )
}

#' @export
format.pre_release_identifier <- function(x, ...) {
  vec_data(x)
}

#' @export
vec_ptype2.pre_release_identifier.pre_release_identifier <- function(
  x,
  y,
  ...
) {
  # If identifier_type differs, promote to alphanumeric
  type_x <- attr(x, "identifier_type")
  type_y <- attr(y, "identifier_type")
  new_type <- if (identical(type_x, type_y)) type_x else "alphanumeric"
  out <- pre_release_identifier()
  attr(out, "identifier_type") <- new_type
  out
}

#' @export
vec_cast.pre_release_identifier.pre_release_identifier <- function(x, to, ...) {
  # If identifier_type differs, promote to alphanumeric
  type_x <- attr(x, "identifier_type")
  type_to <- attr(to, "identifier_type")
  if (!identical(type_x, type_to)) {
    attr(x, "identifier_type") <- "alphanumeric"
  }
  x
}

#' @export
vec_ptype2.pre_release_identifier.logical <- function(x, y, ...) {
  pre_release_identifier()
}
#' @export
vec_ptype2.logical.pre_release_identifier <- function(x, y, ...) {
  pre_release_identifier()
}

#' @export
vec_ptype2.pre_release_identifier.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.pre_release_identifier <- function(x, y, ...) character()

#' @export
vec_ptype2.pre_release_identifier.integer <- function(x, y, ...) {
  pre_release_identifier()
}
#' @export
vec_ptype2.integer.pre_release_identifier <- function(x, y, ...) {
  pre_release_identifier()
}

#' @export
vec_cast.pre_release_identifier.logical <- function(x, to, ...) {
  if (all(is.na(x))) {
    pre_release_identifier(rep(NA_character_, length(x)))
  } else {
    cli::cli_abort("Cannot cast non-NA logical to pre_release_identifier")
  }
}

#' @export
vec_cast.character.pre_release_identifier <- function(x, to, ...) {
  vec_data(x)
}

#' @export
vec_cast.pre_release_identifier.character <- function(x, to, ...) {
  pre_release_identifier(x)
}

#' @export
vec_cast.integer.pre_release_identifier <- function(x, to, ...) {
  vec_data(x) |>
    as.integer() |>
    suppressWarnings()
}

#' @export
vec_cast.pre_release_identifier.integer <- function(x, to, ...) {
  pre_release_identifier(as.character(x))
}

#' @export
vec_cast.double.pre_release_identifier <- function(x, to, ...) {
  vec_data(x) |>
    as.numeric() |>
    suppressWarnings()
}

#' @export
vec_cast.pre_release_identifier.double <- function(x, to, ...) {
  pre_release_identifier(as.character(x))
}

#' @export
vec_proxy_compare.pre_release_identifier <- function(x, ...) {
  type <- attr(x, "identifier_type")
  is_num <- type == "numeric"
  # Impute empty values
  is_empty <- x == ""
  imputed_value <- if (is_num) {
    ifelse(is_empty, -Inf, as.numeric(x))
  } else {
    ifelse(is_empty, "", as.character(x))
  }

  df_list(
    type = !is_num,
    value = imputed_value
  ) |>
    new_data_frame()
}
