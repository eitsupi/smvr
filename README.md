
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smvr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/smvr)](https://CRAN.R-project.org/package=smvr)
<!-- badges: end -->

Simple implementation of [Semantic Versioning
2.0.0](https://semver.org/) on R.

smvr provides a lightweight, fully vectorized R class for Semantic
Versioning. It enables type-safe manipulation, comparison, and sorting
of version numbers, and integrates smoothly with tidyverse tools.

## Installation

Install the latest release version from CRAN:

``` r
install.packages("smvr")
```

The development version can be installed from GitHub:

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

# Can compare with string notation
v[v < "1.0.0"]
#> <smvr[3]>
#> [1] 1.0.0-alpha   1.0.0-alpha.1 0.9.0

# Works with tibble data frame and dplyr
tibble::tibble(version = v) |>
  dplyr::arrange(version) |>
  dplyr::mutate(
    `>= 1.0.0` = version >= "1.0.0",
    `pre-release` = is_pre_release(version),
  )
#> # A tibble: 5 × 3
#>          version `>= 1.0.0` `pre-release`
#>           <smvr> <lgl>      <lgl>        
#> 1          0.9.0 FALSE      FALSE        
#> 2    1.0.0-alpha FALSE      TRUE         
#> 3  1.0.0-alpha.1 FALSE      TRUE         
#> 4          1.0.0 TRUE       FALSE        
#> 5 1.0.1+20250621 TRUE       FALSE
```

## Features

- Fully vectorized Semantic Versioning class.
- Type-safe comparison and sorting.
- Tidyverse compatibility (`{tibble}`, `{dplyr}`, etc.).
- No dependencies except `{vctrs}`.

## Related Works

- The numeric version class vector can be crated with
  `numeric_version()` in base R works well for versions that only have
  MAJOR.MINOR.PATCH. But it does not support pre-release identifiers of
  SemVer, so in the case of including pre-release versions, it is not
  suitable.
- The [semver](https://CRAN.R-project.org/package=semver) package is a
  wrapper for a C++ SemVer parser. The class provided by this package is
  a special list, which does not work well with `{tibble}` and
  `{dplyr}`.
