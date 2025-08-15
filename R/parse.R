#' @param x A character vector representing semantic versions.
#'   Each version should follow the
#'   [Semantic Versioning Specification](https://semver.org/).
#'   Partial matches are not allowed (e.g., `"1.0"` is not valid).
#' @rdname smvr
#' @order 2
#' @export
parse_semver <- function(x) {
  x <- vec_cast(x, character(), call = caller_env())

  pattern <- SEM_VER_PATTERN
  parts <- regmatches(x, regexec(pattern, x))

  invalid <- lengths(parts) == 0L & !is.na(x)

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
    new_vctr(
      class = "parsed_chr_pre_release_ids",
      inherit_base_type = TRUE
    ) |>
    parse_pre_release_ids()
  build <- map_chr(parts, \(x) x[6]) |>
    new_vctr(
      class = "parsed_chr_build_metadata",
      inherit_base_type = TRUE
    )

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
  x <- vec_cast(x, new_parsed_chr_pre_release_ids(), call = call)

  # strsplit returns character(0) for empty strings
  values <- vec_rbind(
    !!!(strsplit(x, "\\.") |> map(\(chr) chr %0% "")),
    .name_repair = "unique_quiet"
  )
  # Fill with empty strings to ensure each row has the same length
  values[is.na(values)] <- ""

  out <- new_pre_release_ids(!!!values)

  # Fix NAs
  out[is.na(x)] <- NA
  out
}

#' Internal class for parsed pre-release identifiers strings
#' @noRd
new_parsed_chr_pre_release_ids <- function(
  x = character(),
  ...,
  call = caller_env()
) {
  x <- vec_cast(x, character())

  invalid <- !grepl(sprintf("^%s$", PRE_RELEASE_IDS_PATTERN), x, perl = TRUE) &
    !is.na(x) &
    nzchar(x)

  if (any(invalid)) {
    cli::cli_warn(
      c(
        `!` = "Invalid pre-release ids detected, setting to {.code NA}.",
        x = "Problematic values: {.val {x[invalid]}}",
        i = "Pre-release ids must match the pattern: {.str ^{PRE_RELEASE_IDS_PATTERN}$}"
      ),
      call = call
    )
  }

  parsed <- x
  parsed[invalid] <- NA_character_

  new_vctr(
    parsed,
    class = "parsed_chr_pre_release_ids",
    inherit_base_type = TRUE
  )
}

#' @export
vec_cast.parsed_chr_pre_release_ids.parsed_chr_pre_release_ids <- function(
  x,
  to,
  ...
) {
  x
}

#' @export
vec_ptype2.character.parsed_chr_pre_release_ids <- function(x, y, ...) {
  character()
}
#' @export
vec_ptype2.parsed_chr_pre_release_ids.character <- function(x, y, ...) {
  character()
}

#' @export
vec_cast.character.parsed_chr_pre_release_ids <- function(x, to, ...) {
  vec_data(x)
}

#' @export
vec_cast.parsed_chr_pre_release_ids.character <- function(
  x,
  to,
  ...,
  call = caller_env()
) {
  new_parsed_chr_pre_release_ids(x, call = call)
}

#' Internal class for parsed pre-release identifiers strings
#' @noRd
new_parsed_chr_build_metadata <- function(
  x = character(),
  ...,
  call = caller_env()
) {
  x <- vec_cast(x, character())

  invalid <- !grepl(sprintf("^%s$", BUILD_METADATA_PATTERN), x, perl = TRUE) &
    !is.na(x) &
    nzchar(x)

  if (any(invalid)) {
    cli::cli_warn(
      c(
        `!` = "Invalid build metadata detected, setting to {.code NA}.",
        x = "Problematic values: {.val {x[invalid]}}",
        i = "Build metadata should have the pattern: {.str ^{BUILD_METADATA_PATTERN}$}"
      ),
      call = call
    )
  }

  parsed <- x
  parsed[invalid] <- NA_character_

  new_vctr(
    parsed,
    class = "parsed_chr_build_metadata",
    inherit_base_type = TRUE
  )
}

#' @export
vec_cast.parsed_chr_build_metadata.parsed_chr_build_metadata <- function(
  x,
  to,
  ...
) {
  x
}

#' @export
vec_ptype2.character.parsed_chr_build_metadata <- function(x, y, ...) {
  character()
}
#' @export
vec_ptype2.parsed_chr_build_metadata.character <- function(x, y, ...) {
  character()
}

#' @export
vec_cast.character.parsed_chr_build_metadata <- function(x, to, ...) {
  vec_data(x)
}

#' @export
vec_cast.parsed_chr_build_metadata.character <- function(
  x,
  to,
  ...,
  call = caller_env()
) {
  new_parsed_chr_build_metadata(x, call = call)
}
