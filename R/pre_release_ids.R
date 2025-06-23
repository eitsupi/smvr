#' Pre-release identifiers
#'
#' @description
#' Create a vector of pre-release identifiers, which can be used for
#' marking versions as pre-release versions.
#'
#' - [pre_release_ids()] is a low-level constructor for creating
#'   pre-release identifiers from individual components.
#' - [parse_pre_release_ids()] parses a character vector into
#'   pre-release identifiers.
#'
#' Empty identifiers are special cases that indicate
#' _not a pre-release version_.
#' @details
#' Internally, [pre_release_ids] store up to 5 [pre_release_identifier].
#' So this can't represent pre-release identifiers with more than 5 components.
#' If passing character containing more than 5 components to
#' [parse_pre_release_ids()], it will throw an error.
#' @param id1,id2,id3,id4,id5 Single pre-release identifiers.
#'   Each identifier can be something to be cast to a [pre_release_identifier]
#'   vector by [vctrs::vec_cast()].
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
#' # Works with base R vectors.
#' ids[ids > "beta.2"]
#' @aliases pre_release_ids
#' @order 1
#' @export
new_pre_release_ids <- function(
  id1 = character(),
  id2 = "",
  id3 = "",
  id4 = "",
  id5 = ""
) {
  # Ensure all ids are same length and type
  values <- df_list(
    id1 = id1,
    id2 = id2,
    id3 = id3,
    id4 = id4,
    id5 = id5
  ) |>
    lapply(\(x) {
      vec_cast(x, new_pre_release_identifier())
    }) |>
    new_data_frame()

  # For each row, check that after the first empty, all subsequent ids are also empty
  for (i in seq_len(nrow(values))) {
    row_ids <- values[i, ]
    first_empty <- which(row_ids == "")[1]
    if (!is.na(first_empty) && first_empty < 5) {
      if (any(row_ids[(first_empty + 1):5] != "")) {
        cli::cli_abort(c(
          "All ids after the first empty must also be empty.",
          x = "Problematic index: {i}"
        ))
      }
    }
  }
  out <- new_rcrd(
    vec_cbind(
      is_empty = values$id1 == new_pre_release_identifier(""),
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
  ids <- format(field(x, "id1"))
  for (i in 2:5) {
    id <- format(field(x, paste0("id", i)))
    ids <- ifelse(
      !is.na(ids) & nzchar(ids) & !is.na(id) & nzchar(id),
      paste0(ids, ".", id),
      ids
    )
  }
  ids
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
  x
}

#' @export
vec_ptype2.logical.pre_release_ids <- function(x, y, ...) new_pre_release_ids()
#' @export
vec_ptype2.pre_release_ids.logical <- function(x, y, ...) new_pre_release_ids()

#' @export
vec_ptype2.character.pre_release_ids <- function(x, y, ...) {
  new_pre_release_ids()
}
#' @export
vec_ptype2.pre_release_ids.character <- function(x, y, ...) {
  new_pre_release_ids()
}

#' @export
vec_cast.pre_release_ids.pre_release_ids <- function(x, to, ...) x

#' @export
vec_cast.pre_release_ids.pre_release_ids <- function(x, to, ...) {
  x
}

#' @export
vec_cast.pre_release_ids.logical <- function(x, to, ...) {
  if (all(is.na(x))) {
    na_id <- new_pre_release_identifier(NA_character_)
    new_pre_release_ids(
      id1 = na_id,
      id2 = na_id,
      id3 = na_id,
      id4 = na_id,
      id5 = na_id
    )
  } else {
    cli::cli_abort("Cannot cast non-NA logical to pre_release_ids")
  }
}

#' @export
vec_cast.character.pre_release_ids <- function(x, to, ...) {
  format(x)
}

#' @export
vec_cast.pre_release_ids.character <- function(x, to, ...) {
  parse_pre_release_ids(x)
}
