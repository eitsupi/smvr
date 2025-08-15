PRE_RELEASE_IDENTIFIER_PATTERN <- r"((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))"

PRE_RELEASE_IDS_PATTERN <- sprintf(
  r"((%s(?:\.%s)*))",
  PRE_RELEASE_IDENTIFIER_PATTERN,
  PRE_RELEASE_IDENTIFIER_PATTERN
)

BUILD_METADATA_PATTERN <- r"(([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))"

SEM_VER_PATTERN <- sprintf(
  r"(^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-%s)?(?:\+%s)?$)",
  PRE_RELEASE_IDS_PATTERN,
  BUILD_METADATA_PATTERN
)

DEFAULT_ID_LENGTH <- 5L
