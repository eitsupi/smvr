
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smvr

<!-- badges: start -->

<!-- badges: end -->

Simple implementation of [Semantic Versioning
2.0.0](https://semver.org/) on R.

smvr provides a `{vctrs}`-based vector class `smvr` for semantic
versions. It enables type-safe manipulation, comparison, and sorting of
semantic versions.

## Installation

You can install the development version of smvr with:

``` r
pak::pak("eitsupi/smvr")
```

## Example

``` r
library(smvr)

# Parse version characters into smvr objects
v <- parse_semver(c("1.0.0", "1.0.0-alpha", "1.0.0-alpha.1", "1.0.1+20250621", "0.9.0"))

# Sort versions
sort(v)
#> <smvr[5]>
#> [1] 0.9.0          1.0.0-alpha    1.0.0-alpha.1  1.0.0          1.0.1+20250621

# Compare versions
smvr(1, 0, 0) < smvr(1, 0, 1)
#> [1] TRUE

# Cast from / to the `numeric_version` class
as_smvr(numeric_version(c("4", "4.5.1")))
#> <smvr[2]>
#> [1] 4.0.0 4.5.1
vctrs::vec_cast(smvr(1, 2, 3), numeric_version(character()))
#> [1] '1.2.3'

# Works with tibble data frame and dplyr
tibble::tibble(version = v) |>
  dplyr::arrange(version) |>
  dplyr::mutate(`>= 1.0.0` = version >= smvr(1))
#> # A tibble: 5 × 2
#>          version `>= 1.0.0`
#>           <smvr> <lgl>     
#> 1          0.9.0 FALSE     
#> 2    1.0.0-alpha FALSE     
#> 3  1.0.0-alpha.1 FALSE     
#> 4          1.0.0 TRUE      
#> 5 1.0.1+20250621 TRUE
```

## Features

- Create version objects with `smvr()`
- Parse version characters with `parse_semver()`
- Use comparison operators (\<, \>, ==), `sort`, `order`, etc.
  (`{vctrs}` compatible)

## Known Limitations

- The number of pre-release identifier fields is limited to 5.
- When comparing vectors, the type for pre-release identifiers is
  determined for the whole vector. This means that if any identifier is
  alphanumerical, all are compared as strings, so numeric comparison may
  not be used for some fields.

``` r
# Only 5 pre-release fields are supported:
try(parse_semver("1.2.3-a.b.c.d.e.f"))
#> Error in parse_semver("1.2.3-a.b.c.d.e.f") : 
#>   Unsupported pre-release identifiers in '1.2.3-a.b.c.d.e.f'.
#> ! Only up to 5 pre-release identifiers are supported, got 6.

# Numeric comparison may not be used if any field is alphanumerical:
# For example, the third value "1.0.0-a.b" make the third pre-release identifier alphabetical,
# so all pre-release identifiers are compared as strings.
prerelease_include_alphabet <- parse_semver(c("1.0.0-a.1", "1.0.0-a.2", "1.0.0-a.b"))
prerelease_include_alphabet[2]
#> <smvr[1]>
#> [1] 1.0.0-a.2
# All are compared as strings, so "10" < "2" (alphabetical order)
prerelease_include_alphabet[2] < parse_semver("1.0.0-a.10")
#> [1] FALSE

# If all pre-release identifiers are numeric, they can be compared numerically:
prerelease_only_numeric <- parse_semver(c("1.0.0-a.1", "1.0.0-a.2", "1.0.0-a.3"))
prerelease_only_numeric[2]
#> <smvr[1]>
#> [1] 1.0.0-a.2
prerelease_only_numeric[2] < parse_semver("1.0.0-a.10")
#> [1] TRUE
```
