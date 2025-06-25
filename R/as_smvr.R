#' Convert to `smvr` vector
#'
#' `as_smvr()` is a generic function that converts an object to `smvr`
#' vector. The default method uses `vctrs::vec_cast()` to convert the object.
#' @inherit smvr return
#' @param x An object to convert to `smvr`.
#' @param ... Additional arguments passed to methods.
#' @examples
#' as_smvr(c("1.0.0", "2.0.0-rc.1", "3.0.0+build.1"))
#' as_smvr(numeric_version(c("1", "2.3")))
#' as_smvr(NA)
#' @export
as_smvr <- function(x, ...) {
  UseMethod("as_smvr")
}

#' @rdname as_smvr
#' @export
as_smvr.default <- function(x, ...) {
  vec_cast(x, smvr(), call = caller_env())
}
