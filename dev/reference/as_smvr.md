# Convert to `smvr` vector

`as_smvr()` is a generic function that converts an object to `smvr`
vector. The default method uses
[`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html)
to convert the object.

## Usage

``` r
as_smvr(x, ...)

# Default S3 method
as_smvr(x, ...)
```

## Arguments

- x:

  An object to convert to `smvr`.

- ...:

  Additional arguments passed to methods.

## Value

A [smvr](https://eitsupi.github.io/smvr/dev/reference/smvr.md) class
vector.

## Examples

``` r
as_smvr(c("1.0.0", "2.0.0-rc.1", "3.0.0+build.1"))
#> <smvr[3]>
#> [1] 1.0.0         2.0.0-rc.1    3.0.0+build.1
as_smvr(numeric_version(c("1", "2.3")))
#> <smvr[2]>
#> [1] 1.0.0 2.3.0
as_smvr(NA)
#> <smvr[1]>
#> [1] <NA>
```
