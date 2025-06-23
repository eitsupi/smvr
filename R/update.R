#' Update version components
#'
#' @description
#' These functions allows to update the components of version objects.
#'
#' - `increment_major()`, `increment_minor()`, and `increment_patch()` update
#'   the major, minor, and patch version numbers respectively.
#' - `mark_as_pre_release()` marks the version as a pre-release version.
#' - `add_build_metadata()` adds build metadata to the version.
#'
#' @param x An version object
#' @param ... Additional arguments passed to methods.
#' @param ids A character vector of pre-release identifiers.
#' @param metadata A character vector of build metadata.
#' @return An updated version object with the specified changes applied.
#' @examples
#' v <- parse_semver(c("0.9.9", "1.0.0-a.1", "1.1.0+1"))
#'
#' increment_major(v)
#' increment_minor(v)
#' increment_patch(v)
#' mark_as_pre_release(v, ids = c("pre.1"))
#' add_build_metadata(v, metadata = "build.1")
#' @name update-version
NULL

#' @rdname update-version
#' @export
increment_major <- function(x, ...) {
  UseMethod("increment_major")
}

#' @rdname update-version
#' @export
increment_major.smvr <- function(x, ...) {
  core <- field(x, "version_core")
  smvr(
    major = core$major + 1L,
    minor = 0L,
    patch = 0L,
    pre_release = "",
    build = ""
  )
}

#' @rdname update-version
#' @export
increment_minor <- function(x, ...) {
  UseMethod("increment_minor")
}

#' @rdname update-version
#' @export
increment_minor.smvr <- function(x, ...) {
  core <- field(x, "version_core")
  smvr(
    major = core$major,
    minor = core$minor + 1L,
    patch = 0L,
    pre_release = "",
    build = ""
  )
}

#' @rdname update-version
#' @export
increment_patch <- function(x, ...) {
  UseMethod("increment_patch")
}

#' @rdname update-version
#' @export
increment_patch.smvr <- function(x, ...) {
  core <- field(x, "version_core")
  smvr(
    major = core$major,
    minor = core$minor,
    patch = core$patch + 1L,
    pre_release = "",
    build = ""
  )
}

#' @rdname update-version
#' @export
mark_as_pre_release <- function(x, ...) {
  UseMethod("mark_as_pre_release")
}

#' @rdname update-version
#' @export
mark_as_pre_release.smvr <- function(x, ids = "pre", ...) {
  ids <- vec_cast(ids, new_pre_release_ids())
  if (anyNA(ids)) {
    cli::cli_abort(
      c(
        "Marking as pre-release failed.",
        x = "Invalid pre-release identifiers: {.val {ids[is.na(ids)]}}",
        i = "{.code NA} values are not allowed in {.arg ids}."
      )
    )
  }
  # Recycle ids if the length is 1
  field(x, "pre_release") <- if (length(ids) == 1L) {
    rep(ids, length(x))
  } else {
    ids
  }
  x
}

#' @rdname update-version
#' @export
add_build_metadata <- function(x, ...) {
  UseMethod("add_build_metadata")
}

#' @rdname update-version
#' @export
add_build_metadata.smvr <- function(x, metadata = "", ...) {
  metadata <- vec_cast(metadata, character())
  if (anyNA(metadata)) {
    cli::cli_abort(
      c(
        "Adding build metadata failed.",
        x = "Invalid metadata: {.val {metadata[is.na(metadata)]}}",
        i = "{.code NA} values are not allowed in {.arg metadata}."
      )
    )
  }
  # Check the build metadata pattern
  if (
    any(
      !grepl(BUILD_METADATA_PATTERN, metadata, perl = TRUE) &
        nzchar(metadata) &
        !is.na(metadata)
    )
  ) {
    problematic_builds <- metadata[
      !grepl(BUILD_METADATA_PATTERN, metadata, perl = TRUE) &
        nzchar(metadata) &
        !is.na(metadata)
    ]
    cli::cli_abort(
      c(
        "{.arg metadata} must match the pattern {.str {BUILD_METADATA_PATTERN}}.",
        x = "Problematic values: {.val {problematic_builds}}"
      )
    )
  }
  # Recycle metadata if the length is 1
  field(x, "build") <- if (length(metadata) == 1L) {
    rep(metadata, length(x))
  } else {
    metadata
  }
  x
}
