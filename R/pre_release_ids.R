#' Pre-release identifiers
#'
#' @description
#' A class representing a collection of [identifiers][pre_release_identifier],
#' which are used for representing pre-release versions.
#'
#' There are two functions to create the [pre_release_ids] vector:
#'
#' - [pre_release_ids()] is a low-level constructor for creating
#'   pre-release identifiers from individual components.
#' - [parse_pre_release_ids()] parses a character vector into
#'   pre-release identifiers.
#'
#' @details
#' If the components are empty, they are treated as the highest precedence
#' pre-release ids, which is used to indicate that the version is _not
#' a pre-release version_.
#' @section Limitations:
#' There are some limitations on the number of identifiers
#' in some operations:
#'
#' - When comparing with a string, the number of identifiers in the string.
#'   If it exceeds 5, an error is raised.
#' - When assigning, the number of identifiers in the value being assigned.
#'   If it exceeds the number of identifiers in the target or 5,
#'   whichever is larger, an error is raised.
#'
#' Please refer to the examples for details.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>
#'   Single pre-release identifiers.
#'   Each identifier can be something to be cast to a [pre_release_identifier]
#'   vector by [vctrs::vec_cast()].
#'   All components must be of the same length or length 1 (will be recycled).
#' @return A [pre_release_ids] vector.
#' @examples
#' # Each components are concatenated with a dot
#' new_pre_release_ids("rc", 1:3)
#'
#' ids <- parse_pre_release_ids(
#'   c("", "alpha.beta", "alpha.1", "beta", "beta.11", "beta.2")
#' )
#' ids
#'
#' # Empty ids have the highest precedence
#' # (Used to indicate not a pre-release version)
#' vctrs::vec_sort(ids)
#'
#' # Can be compared with string notation
#' ids[ids > "beta.2"]
#'
#' # Limitations:
#' # 1. When comparing with a string, the number of identifiers in the string
#' #    must not exceed 5.
#' try(ids[ids > "beta.2.3.4.5.6"])
#'
#' # This works since the string is parsed first.
#' ids[ids > parse_pre_release_ids("beta.2.3.4.5.6")]
#'
#' # 2. When assigning, the number of identifiers in the value being assigned
#' #    must not exceed the number of identifiers in the target or 5,
#' #    whichever is larger.
#' try(ids[1] <- parse_pre_release_ids("beta.2.3.4.5.6"))
#' @name pre_release_ids
NULL

#' @rdname pre_release_ids
#' @order 1
#' @export
new_pre_release_ids <- function(...) {
  check_dots_unnamed()

  dots <- list2(...) |>
    lapply(\(x) {
      vec_cast(x, new_pre_release_identifier())
    })

  # Ensure having 5 components at least
  # because of assigning more than the components
  # is not allowed.
  n_dots <- length(dots)
  filling <- if (n_dots != 0L && n_dots < DEFAULT_ID_LENGTH) {
    vec_rep(new_pre_release_identifier(""), DEFAULT_ID_LENGTH - n_dots)
  } else {
    new_pre_release_identifier()
  }

  # Ensure all ids are same length and type
  values <- df_list(!!!dots, !!!filling, .name_repair = "minimal") |>
    (\(x) set_names(x, sprintf("id%d", seq_along(x))))() |>
    new_data_frame()

  if (n_dots > 1L) {
    # For each row, check that after the first empty,
    # all subsequent ids are also empty
    dotted <- inject(paste("", !!!format(values), sep = "."))
    # Detect any non-empty ID following an empty component.
    violated <- grepl("\\.{2,}[^.]", dotted)

    if (any(violated, na.rm = TRUE)) {
      cli::cli_abort(c(
        "All ids after the first empty must also be empty.",
        x = "Problematic indices: {.val {which(violated)}}"
      ))
    }
  }

  out <- new_rcrd(
    vec_cbind(
      is_empty = (values$id1 %||% new_pre_release_identifier()) ==
        new_pre_release_identifier(""),
      values
    ),
    class = "pre_release_ids"
  )

  # Fix NAs
  out[!vec_detect_complete(vec_data(out))] <- NA
  out
}

#' @export
format.pre_release_ids <- function(x, ...) {
  vec_data(x)[, -1L] |>
    format() |>
    reduce(\(acc, nxt) {
      ifelse(
        !is.na(acc) & nzchar(acc) & !is.na(nxt) & nzchar(nxt),
        sprintf("%s.%s", acc, nxt),
        acc
      )
    })
}

#' @export
print.pre_release_ids <- function(x, ...) {
  formatted <- format(x)

  is_empty <- field(x, "is_empty")
  formatted[is_empty] <- "<empty>"
  formatted[is.na(x)] <- "<NA>"

  cat(formatted %0% "new_pre_release_ids()", sep = "\n")
  invisible(x)
}

#' @export
vec_ptype2.pre_release_ids.pre_release_ids <- function(
  x,
  y,
  ...
) {
  if (ncol(vec_data(x)) >= ncol(vec_data(y))) {
    x
  } else {
    y
  }
}

#' @export
vec_ptype2.logical.pre_release_ids <- function(x, y, ...) new_pre_release_ids()
#' @export
vec_ptype2.pre_release_ids.logical <- function(x, y, ...) new_pre_release_ids()

#' @export
vec_ptype2.character.pre_release_ids <- function(x, y, ...) {
  ptype2_chr_ids_impl(x, y)
}
#' @export
vec_ptype2.pre_release_ids.character <- function(x, y, ...) {
  ptype2_chr_ids_impl(y, x)
}

ptype2_chr_ids_impl <- function(chr, ids) {
  # We can't determine the number of identifiers included in the
  # character vector, so only 5 identifiers are allowed by default.
  # Or, if the ids have more identifiers, allows that length.
  n_ids <- ncol(vec_data(ids)) - 1L
  new_pre_release_ids(
    !!!vec_rep(new_pre_release_identifier(), max(DEFAULT_ID_LENGTH, n_ids))
  )[0L]
}

#' @export
vec_cast.pre_release_ids.pre_release_ids <- function(x, to, ...) {
  width_id_x <- ncol(vec_data(x)) - 1L
  width_id_to <- ncol(vec_data(to)) - 1L
  if (width_id_x >= width_id_to) {
    x
  } else {
    out <- vec_data(x) |>
      vec_cbind(
        !!!vec_rep(new_pre_release_identifier(""), width_id_to - width_id_x),
        .name_repair = "minimal"
      ) |>
      set_names(
        c(
          "is_empty",
          sprintf("id%d", seq_len(width_id_to))
        )
      ) |>
      vec_restore(to)

    # Fix NAs
    out[!vec_detect_complete(vec_data(out))] <- NA
    out
  }
}

#' @export
vec_cast.pre_release_ids.logical <- function(x, to, ...) {
  if (all(is.na(x))) {
    new_pre_release_ids(new_pre_release_identifier(x))
  } else {
    cli::cli_abort("Cannot cast non-NA logical to {.cls pre_release_ids}")
  }
}

#' @export
vec_cast.character.pre_release_ids <- function(x, to, ...) {
  format(x)
}

#' @export
vec_cast.pre_release_ids.character <- function(x, to, ...) {
  parse_pre_release_ids(x) |>
    vec_cast(to)
}

#' @export
vec_cast.pre_release_ids.parsed_chr_pre_release_ids <- vec_cast.pre_release_ids.character
