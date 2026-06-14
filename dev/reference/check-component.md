# Check if the `smvr` object has a specific component

These functions check if the
[smvr](https://eitsupi.github.io/smvr/dev/reference/smvr.md) object has
a specific component.

- `is_pre_release()`: Checks if the pre-release identifiers are present.

- `has_build_metadata()`: Checks if the build metadata is present.

## Usage

``` r
is_pre_release(x)

has_build_metadata(x)
```

## Arguments

- x:

  A [smvr](https://eitsupi.github.io/smvr/dev/reference/smvr.md) object.

## Value

Indicates whether `x` has the specified component.

## See also

- [extract-component](https://eitsupi.github.io/smvr/dev/reference/extract-component.md)
  functions for extracting components from a
  [smvr](https://eitsupi.github.io/smvr/dev/reference/smvr.md) object.

## Examples

``` r
v <- parse_semver(c(
  "1.0.0", "2.0.0-alpha", "2.0.0-beta", "2.0.0-beta.2+build.123"
))
v
#> <smvr[4]>
#> [1] 1.0.0                  2.0.0-alpha            2.0.0-beta            
#> [4] 2.0.0-beta.2+build.123

is_pre_release(v)
#> [1] FALSE  TRUE  TRUE  TRUE
has_build_metadata(v)
#> [1] FALSE FALSE FALSE  TRUE
```
