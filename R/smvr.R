#' smvr: Semantic Versioning 2.0 vector class
#'
#' @description
#' A vctrs-based vector class representing Semantic Versioning 2.0.
#' @param major Major version (integer)
#' @param minor Minor version (integer, default: 0)
#' @param patch Patch version (integer, default: 0)
#' @param pre_release Pre-release identifiers (default: empty string)
#' @param build Build string (default: empty string)
#' @return An object of class smvr
#' @export
smvr <- function(
  major = integer(),
  minor = 0L,
  patch = 0L,
  pre_release = "",
  build = ""
) {
  major <- vec_cast(major, integer())
  minor <- vec_cast(minor, integer())
  patch <- vec_cast(patch, integer())
  pre_release <- vec_cast(pre_release, pre_release_ids())
  build <- vec_cast(build, character())

  if (any(major < 0L, na.rm = TRUE)) {
    cli::cli_abort(
      c(
        "{.code major} must be non-negative integer values.",
        x = "Problematic values: {.val {major[major < 0L]}}"
      )
    )
  }
  if (any(minor < 0L, na.rm = TRUE)) {
    cli::cli_abort(
      c(
        "{.code minor} must be non-negative integer values.",
        x = "Problematic values: {.val {minor[minor < 0L]}}"
      )
    )
  }
  if (any(patch < 0L, na.rm = TRUE)) {
    cli::cli_abort(
      c(
        "{.code patch} must be non-negative integer values.",
        x = "Problematic values: {.val {patch[patch < 0L]}}"
      )
    )
  }

  # Use vctrs::df_list to recycle and align lengths
  version_core <- vctrs::df_list(
    major = major,
    minor = minor,
    patch = patch
  ) |>
    new_data_frame()

  values <- vctrs::df_list(
    version_core = version_core,
    pre_release = pre_release,
    build = build
  ) |>
    new_data_frame()

  out <- vctrs::new_rcrd(
    fields = values,
    class = "smvr"
  )

  # Fix NAs
  out[!vec_detect_complete(vec_data(out))] <- NA
  out
}

#' @export
format.smvr <- function(x, ...) {
  core <- field(x, "version_core")
  pre <- vctrs::field(x, "pre_release")
  build <- vctrs::field(x, "build")
  pre_str <- format(pre)
  res <- sprintf(
    "%d.%d.%d",
    core$major,
    core$minor,
    core$patch
  )
  has_pre <- !is.na(pre_str) & nzchar(pre_str)
  has_build <- !is.na(build) & nzchar(build)

  res[is.na(core$major)] <- NA_character_
  res[has_pre & !is.na(res)] <- paste0(res, "-", pre_str)[has_pre & !is.na(res)]
  res[has_build & !is.na(res)] <- paste0(res, "+", build)[
    has_build & !is.na(res)
  ]
  res
}

#' @export
vec_ptype_abbr.smvr <- function(x, ...) {
  "smvr"
}

#' @export
vec_ptype_full.smvr <- function(x, ...) {
  "smvr"
}

#' @export
vec_ptype2.smvr.smvr <- function(x, y, ...) {
  smvr(
    pre_release = vec_ptype2(
      vctrs::field(x, "pre_release"),
      vctrs::field(y, "pre_release")
    )
  )
}

#' @export
vec_ptype2.logical.smvr <- function(x, y, ...) smvr()
#' @export
vec_ptype2.smvr.logical <- function(x, y, ...) smvr()

#' @export
vec_ptype2.character.smvr <- function(x, y, ...) character()
#' @export
vec_ptype2.smvr.character <- function(x, y, ...) character()

#' @export
vec_cast.smvr.smvr <- function(x, to, ...) {
  version_core <- vctrs::field(x, "version_core")
  smvr(
    major = version_core$major,
    minor = version_core$minor,
    patch = version_core$patch,
    pre_release = vec_cast(
      field(x, "pre_release"),
      field(to, "pre_release")
    ),
    build = field(x, "build")
  )
}

#' @export
vec_cast.smvr.logical <- function(x, to, ...) {
  if (all(is.na(x))) {
    smvr(x)
  } else {
    cli::cli_abort("Cannot cast non-NA logical to smvr")
  }
}

#' @export
vec_cast.character.smvr <- function(x, to, ...) {
  format(x)
}

#' @export
vec_cast.smvr.character <- function(x, to, ...) {
  parse_semver(x)
}

#' @export
vec_proxy_compare.smvr <- function(x, ...) {
  vec_data(x)[, -3] # Build metadata is not used for ordering
}

#' @export
vec_proxy_order.smvr <- function(x, ...) {
  vec_data(x)[, -3] # Build metadata is not used for ordering
}
