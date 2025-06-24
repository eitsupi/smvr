numver_extract_nth <- function(x, n) {
  nums <- unclass(x) |>
    map_int(\(x) x[n]) %|%
    0L

  # Restore NAs
  nums[is.na(x)] <- NA_integer_
  nums
}

#' @rdname extract-component
#' @export
extract_major.numeric_version <- function(x, ...) {
  numver_extract_nth(x, 1L)
}

#' @rdname extract-component
#' @export
extract_minor.numeric_version <- function(x, ...) {
  numver_extract_nth(x, 2L)
}

#' @rdname extract-component
#' @export
extract_patch.numeric_version <- function(x, ...) {
  numver_extract_nth(x, 3L)
}
