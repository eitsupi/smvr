#' A vector representing versions following Semantic Versioning
#'
#' @description
#' The `smvr` class represents versions that follow the
#' [Semantic Versioning Specification (SemVer)](https://semver.org/).
#' A version number contains three components, `MAJOR.MINOR.PATCH`,
#' and optional pre-release and build metadata labels.
#'
#' This is similar to the base R's [numeric_version] class, but always has
#' three components (major, minor, patch) and supports pre-release
#' and build metadata labels. And, unlike [numeric_version],
#' SemVer uses dots (`.`) as separators and does not allow hyphens (`-`)
#' except to indicate the start of a pre-release label.
#'
#' There are two functions to create [smvr] objects:
#'
#' - [smvr()] is a constructor from each component.
#'   Each component must have the same length or length 1 (will be recycled).
#' - [parse_semver()] parses a character vector.
#'
#' @details
#' Build metadata is not used for ordering, but the `==` and `!=` operators
#' check it and exactly same build metadata is required for equality.
#' The other operators (`<`, `<=`, `>`, `>=`) ignore build metadata.
#' @param major,minor,patch Non-negative integers representing
#'   the major, minor, and patch version components.
#'   The default values for `minor` and `patch` are `0`.
#' @param pre_release Something that can be cast to a [pre_release_ids] vector.
#'   This can be empty (`""`) meaning non pre-release (default).
#' @param build Optional build metadata character vector.
#'   Should have the pattern `^[a-zA-Z0-9-]+` and can contain
#'   multiple components separated by dots (`.`).
#'   This can be empty (`""`) meaning no build metadata (default).
#' @return A [smvr] class vector.
#' @seealso
#' - [as_smvr()] to convert other classes to [smvr].
#' - [extract-component] functions to extract components of a [smvr] object.
#'   (Operations opposite to [smvr()]).
#' - [update-version] functions to update components of a [smvr] object.
#' @examples
#' # SemVer versions from components
#' smvr(4, 1:5)
#'
#' # Parse SemVer versions from character
#' v <- parse_semver(c(
#'   "1.0.0",
#'   "1.0.0-alpha",
#'   "1.0.0-beta",
#'   "1.0.0-rc.2",
#'   "1.0.0-rc.10",
#'   NA
#' ))
#' v
#'
#' # Sorting
#' vctrs::vec_sort(v)
#'
#' # Can be compared with string notation
#' v[v >= "1.0.0-rc.2" & !is.na(v)]
#'
#' # Partial version components are treated as NA
#' suppressWarnings(parse_semver("1.5"))
#'
#' # The numeric_version class supports versions with
#' # less than 3 components, and can be cast to smvr.
#' numeric_version("1.5") |>
#'   vctrs::vec_cast(smvr())
#'
#' # Be careful with hyphens in numeric_version and SemVer.
#' # The following examples yield opposite results.
#' numeric_version("1.0.0-1") > "1.0.0" # 1.0.0-1 is the same as 1.0.0.1
#' parse_semver("1.0.0-1") > "1.0.0"    # 1.0.0-1 is a pre-release version
#' @order 1
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
  pre_release <- vec_cast(pre_release, new_pre_release_ids())
  build <- vec_cast(build, new_parsed_chr_build_metadata())

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

  # Use df_list to recycle and align lengths
  version_core <- df_list(
    major = major,
    minor = minor,
    patch = patch
  ) |>
    new_data_frame()

  values <- df_list(
    version_core = version_core,
    pre_release = pre_release,
    build = build
  ) |>
    new_data_frame()

  out <- new_rcrd(
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
  pre <- field(x, "pre_release")
  build <- field(x, "build")
  pre_str <- format(pre)
  res <- sprintf(
    "%d.%d.%d",
    core$major,
    core$minor,
    core$patch
  )
  has_pre <- !field(pre, "is_empty")
  has_build <- nzchar(build, keepNA = TRUE)

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
      field(x, "pre_release"),
      field(y, "pre_release")
    )
  )
}

#' @export
vec_ptype2.logical.smvr <- function(x, y, ...) smvr()
#' @export
vec_ptype2.smvr.logical <- function(x, y, ...) smvr()

# We can't determine the number of identifiers included in the
# character vector, so only 5 identifiers are allowed.
#' @export
vec_ptype2.character.smvr <- function(x, y, ...) {
  smvr(pre_release = ptype2_chr_ids_impl(x, y))
}
#' @export
vec_ptype2.smvr.character <- function(x, y, ...) {
  smvr(pre_release = ptype2_chr_ids_impl(y, x))
}

#' @export
vec_ptype2.numeric_version.smvr <- function(x, y, ...) {
  numeric_version(character())
}
#' @export
vec_ptype2.smvr.numeric_version <- function(x, y, ...) {
  numeric_version(character())
}

#' @export
vec_cast.smvr.smvr <- function(x, to, ...) {
  version_core <- field(x, "version_core")
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
    cli::cli_abort("Cannot cast non-NA logical to {.cls smvr}")
  }
}

#' @export
vec_cast.character.smvr <- function(x, to, ...) {
  format(x)
}

#' @export
vec_cast.smvr.character <- function(x, to, ...) {
  parse_semver(x) |>
    vec_cast(to)
}

#' @export
vec_cast.numeric_version.smvr <- function(x, to, ...) {
  if (any(is_pre_release(x), na.rm = TRUE)) {
    cli::cli_abort(
      c(
        "Pre-release {.cls smvr} cannot be cast to {.cls numeric_version}.",
        x = "Problematic values: {.val {x[is_pre_release(x)]}}"
      )
    )
  }
  add_build_metadata(x, "") |>
    format() |>
    numeric_version(strict = FALSE)
}

#' @export
vec_cast.smvr.numeric_version <- function(x, to, ...) {
  values <- unclass(x)
  n_components <- values |>
    lengths()

  if (any(n_components > 3L)) {
    cli::cli_abort(
      c(
        "Cannot cast {.cls numeric_version} with more than 3 components to {.cls smvr}.",
        x = "Problematic values: {.val {x[n_components > 3L]}}"
      )
    )
  }

  major <- map_int(values, \(x) x[1L]) %|% 0L
  minor <- map_int(values, \(x) x[2L]) %|% 0L
  patch <- map_int(values, \(x) x[3L]) %|% 0L

  out <- smvr(major, minor, patch)

  # Fix NAs
  out[n_components == 0L] <- NA
  out
}

#' @export
vec_proxy_compare.smvr <- function(x, ...) {
  vec_data(x)[, -3] # Build metadata is not used for ordering
}
