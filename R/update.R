#' Update version components
#'
#' @description
#' These functions allows to update the components of version objects.
#'
#' - `increment_major()`, `increment_minor()`, and `increment_patch()` update
#'   the major, minor, and patch version numbers respectively.
#'   Note that these functions reset the pre-release and build metadata to empty.
#' - `mark_as_pre_release()` marks the version as a pre-release version.
#' - `add_build_metadata()` adds build metadata to the version.
#'
#' @param x An version object
#' @param ... Additional arguments passed to methods.
#' @param ids Something can be cast to [pre_release_ids] representing the
#'   pre-release identifiers, length must be 1 or the same as `x`.
#' @param metadata A character vector of build metadata,
#'   length must be 1 or the same as `x`.
#' @return An updated version object with the specified changes applied.
#' @examples
#' v <- parse_semver(c("0.9.9", "1.0.0-a.1", "1.1.0+1"))
#'
#' increment_major(v)
#' increment_minor(v)
#' increment_patch(v)
#' mark_as_pre_release(v, ids = "rc.1")
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
mark_as_pre_release.smvr <- function(x, ids, ...) {
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
    vec_rep(ids, length(x))
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
  parsed <- suppressWarnings(vec_cast(
    metadata,
    new_parsed_chr_build_metadata()
  ))

  if (anyNA(parsed)) {
    cli::cli_abort(
      c(
        "Adding build metadata failed.",
        x = "Invalid metadata: {.val {metadata[is.na(parsed)]}}",
        i = "Invalid pattern or {.code NA} values are not allowed."
      )
    )
  }

  # Recycle metadata if the length is 1
  field(x, "build") <- if (length(parsed) == 1L) {
    vec_rep(parsed, length(x))
  } else {
    parsed
  }
  x
}
