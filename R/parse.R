#' @param x A character vector representing semantic versions.
#'   Each version should follow the
#'   [Semantic Versioning Specification](https://semver.org/).
#'   Partial matches are not allowed (e.g., `"1.0"` is not valid).
#' @rdname smvr
#' @order 2
#' @export
parse_semver <- function(x) {
  x <- vec_cast(x, character(), call = caller_env())

  pattern <- paste0(
    "^",
    "(0|[1-9][0-9]*)\\.", # major
    "(0|[1-9][0-9]*)\\.", # minor
    "(0|[1-9][0-9]*)", # patch
    "(?:-([0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*))?", # pre-release
    "(?:\\+([0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*))?", # build
    "$"
  )
  m <- regexec(pattern, x)
  parts <- regmatches(x, m)
  # If parsing fails, parts will be empty list or have length 1
  valid <- vapply(parts, function(p) length(p) > 0, logical(1))
  major <- minor <- patch <- rep(NA_integer_, length(x))
  pre <- vector("list", length(x))
  build <- rep(NA_character_, length(x))
  for (i in seq_along(x)) {
    if (valid[i]) {
      major[i] <- as.integer(parts[[i]][2])
      minor[i] <- as.integer(parts[[i]][3])
      patch[i] <- as.integer(parts[[i]][4])
      pre_str <- parts[[i]][5]
      if (!is.na(pre_str) && nzchar(pre_str)) {
        ids <- strsplit(pre_str, "\\.")[[1]]
        # Ensure we have exactly 5 identifiers, filling with empty ones if needed
        if (length(ids) > 5) {
          cli::cli_abort(c(
            "Unsupported pre-release identifiers in '{x[i]}'.",
            `!` = "Only up to 5 pre-release identifiers are supported, got {length(ids)}."
          ))
        }
        ids <- c(ids, rep("", 5 - length(ids)))
        pre[[i]] <- do.call(new_pre_release_ids, as.list(ids))
      } else {
        pre[[i]] <- new_pre_release_ids(new_pre_release_identifier(""))
      }
      build[i] <- if (length(parts[[i]]) >= 6) parts[[i]][6] else NA_character_
    } else {
      if (!is.na(x[i])) {
        cli::cli_warn(c(
          `!` = "Cannot parse '{x[i]}' as semantic version, setting to {.code NA}."
        ))
      }
      pre[[i]] <- new_pre_release_ids(new_pre_release_identifier(""))
    }
  }
  smvr(
    major = major,
    minor = minor,
    patch = patch,
    pre_release = vec_c(!!!pre),
    build = build
  )
}

#' @param x A character vector representing pre-release identifiers.
#'   Each identifier separated by a dot (`"."`) will be parsed as a
#'   [pre_release_identifier].
#' @rdname new_pre_release_ids
#' @order 2
#' @export
parse_pre_release_ids <- function(x) {
  x <- vec_cast(x, character(), call = caller_env())

  pattern <- "^([0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*)$"
  m <- regexec(pattern, x)
  parts <- regmatches(x, m)
  pre <- vector("list", length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      pre[[i]] <- new_pre_release_ids(NA)
    } else if (length(parts[[i]]) == 0) {
      if (nzchar(x[i])) {
        cli::cli_warn(c(
          `!` = "Cannot parse '{x[i]}' as pre-release identifiers, setting to {.code NA}."
        ))
        pre[[i]] <- new_pre_release_ids(NA)
      } else {
        pre[[i]] <- new_pre_release_ids("")
      }
    } else {
      ids <- strsplit(parts[[i]][1], "\\.")[[1]]
      # Ensure we have exactly 5 identifiers, filling with empty ones if needed
      length_ids <- length(ids)
      if (length_ids > 5) {
        cli::cli_abort(c(
          "Unsupported pre-release identifiers in '{x[i]}'.",
          `!` = "Only up to 5 pre-release identifiers are supported, got {length_ids}."
        ))
      }
      ids <- c(ids, rep("", 5 - length_ids))
      pre[[i]] <- do.call(new_pre_release_ids, as.list(ids))
    }
  }
  vec_c(!!!pre)
}
