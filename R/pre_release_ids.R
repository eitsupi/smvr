#' pre_release_ids: Pre-release identifiers for Semantic Versioning
#'
#' @description
#' An rcrd class representing up to 5 pre-release identifiers for Semantic Versioning 2.0.
#' @param id1 First identifier (character/integer/pre_release_identifier, default: NA)
#' @param id2 Second identifier (character/integer/pre_release_identifier, default: NA)
#' @param id3 Third identifier (character/integer/pre_release_identifier, default: NA)
#' @param id4 Fourth identifier (character/integer/pre_release_identifier, default: NA)
#' @param id5 Fifth identifier (character/integer/pre_release_identifier, default: NA)
#' @return An object of class pre_release_ids
#' @export
new_pre_release_ids <- function(
  id1 = new_pre_release_identifier(),
  id2 = new_pre_release_identifier(""),
  id3 = new_pre_release_identifier(""),
  id4 = new_pre_release_identifier(""),
  id5 = new_pre_release_identifier("")
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
  out <- new_rcrd(values, class = "pre_release_ids")

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
vec_ptype2.character.pre_release_ids <- function(x, y, ...) character()
#' @export
vec_ptype2.pre_release_ids.character <- function(x, y, ...) character()

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

#' @export
vec_proxy_compare.pre_release_ids <- function(x, ...) {
  vec_cbind(
    is_empty = field(x, "id1") == "",
    vec_data(x)
  )
}
