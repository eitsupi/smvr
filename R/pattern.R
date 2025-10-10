PRE_RELEASE_IDENTIFIER_PATTERN <- r"((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))"

PRE_RELEASE_IDS_PATTERN <- sprintf(
  r"((%s(?:\.%s)*))",
  PRE_RELEASE_IDENTIFIER_PATTERN,
  PRE_RELEASE_IDENTIFIER_PATTERN
)

BUILD_METADATA_PATTERN <- r"(([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))"

#' A suggested regular expression (RegEx) to check a SemVer string
#'
#' This is a suggested regular expression (RegEx) to check a SemVer string,
#' without the `"^"` at the start and `"$"` at the end.
#' It is useful to extract the SemVer components from VCS tags etc.
#' @return Single string
#' @export
#' @examples
#' SEM_VER_PATTERN
#'
#' # VCS tag names often have a "v" prefix to SemVer
#' tag_names <- c("v1.0.0", "v1.1.0-alpha.1", "v1.1.0+build.1", "not-a-version")
#'
#' # Extract and parse SemVer
#' regmatches(tag_names, m = regexpr(SEM_VER_PATTERN, tag_names)) |>
#'   parse_semver()
SEM_VER_PATTERN <- sprintf(
  r"((0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-%s)?(?:\+%s)?)",
  PRE_RELEASE_IDS_PATTERN,
  BUILD_METADATA_PATTERN
)
