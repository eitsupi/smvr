#' Single pre-release identifier
#'
#' A class representing a single pre-release identifier
#' (alphanumeric or numeric) for Semantic Versioning 2.0.0.
#' @param x Something that can be coerced to a character vector by
#'   [vctrs::vec_cast()]. Empty string (`""`) is a special case
#'   that means no identifier. The default is length 0 character.
#' @return A [pre_release_identifier] vector.
#' @seealso
#' - [pre_release_ids]: Whole pre-release identifiers
#'   (Concatenation of [pre_release_identifier]).
#' @examples
#' ids <- new_pre_release_identifier(
#'   c("1", "2", "10", "01", "alpha", "beta", "", NA)
#' )
#' ids
#'
#' # Numeric identifiers are always sorted before alphanumeric ones.
#' vctrs::vec_sort(ids)
#'
#' # Works with base R vectors.
#' ids[ids == "alpha" & !is.na(ids)]
#' ids[ids > 2L & !is.na(ids)]
#' @aliases pre_release_identifier
#' @export
new_pre_release_identifier <- function(x = character()) {
  x <- vec_cast(x, character(), call = caller_env())

  # Only [0-9a-zA-Z-]* allowed
  invalid <- !is.na(x) & !grepl("^[0-9a-zA-Z-]*$", x)
  if (any(invalid)) {
    cli::cli_abort(
      c(
        "Identifier must comprise only ASCII alphanumerics and hyphens {.str [0-9a-zA-Z-]}.",
        x = "Invalid values: {.val {x[invalid]}}"
      )
    )
  }

  is_empty <- !nzchar(x)
  is_num <- grepl("^[0-9]+$", x) & !grepl("^0[0-9]+", x)

  alphanumeric_id <- x
  numeric_id <- double(length(x))

  # Numeric identifiers always have lower precedence than
  # non-numeric identifiers
  numeric_id[!is_num & !is_empty] <- Inf
  numeric_id[is_num] <- as.numeric(x[is_num])
  numeric_id[is_empty] <- -1

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
print.pre_release_identifier <- function(x, ...) {
  formatted <- format(x)

  num_field <- field(x, "numeric")
  is_alphanumeric <- is.infinite(num_field)
  is_empty <- num_field < 0
  is_num <- is.finite(num_field) & !is_empty

  formatted[is_alphanumeric] <- paste0(
    formatted[is_alphanumeric],
    " <alphanumeric>"
  )
  formatted[is_num] <- paste0(
    formatted[is_num],
    " <numeric>"
  )
  formatted[is_empty] <- "<empty>"
  formatted[is.na(x)] <- "<NA>"

  cat(formatted %0% "new_pre_release_identifier()", sep = "\n")
  invisible(x)
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
vec_ptype2.pre_release_identifier.character <- function(x, y, ...) {
  new_pre_release_identifier()
}
#' @export
vec_ptype2.character.pre_release_identifier <- function(x, y, ...) {
  new_pre_release_identifier()
}

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
  field(x, "alphanumeric") |>
    as.integer() |>
    suppressWarnings()
}

#' @export
vec_cast.pre_release_identifier.integer <- function(x, to, ...) {
  new_pre_release_identifier(as.character(x))
}

#' @export
vec_cast.double.pre_release_identifier <- function(x, to, ...) {
  field(x, "alphanumeric") |>
    as.numeric() |>
    suppressWarnings()
}

#' @export
vec_cast.pre_release_identifier.double <- function(x, to, ...) {
  new_pre_release_identifier(as.character(x))
}
