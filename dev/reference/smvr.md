# A vector representing versions following Semantic Versioning

The `smvr` class represents versions that follow the [Semantic
Versioning Specification (SemVer)](https://semver.org/). A version
number contains three components, `MAJOR.MINOR.PATCH`, and optional
pre-release and build metadata labels.

This is similar to the base R's
[numeric_version](https://rdrr.io/r/base/numeric_version.html) class,
but always has three components (major, minor, patch) and supports
pre-release and build metadata labels. And, unlike
[numeric_version](https://rdrr.io/r/base/numeric_version.html), SemVer
uses dots (`.`) as separators and does not allow hyphens (`-`) except to
indicate the start of a pre-release label.

There are two functions to create smvr objects:

- `smvr()` is a constructor from each component. Each component must
  have the same length or length 1 (will be recycled).

- `parse_semver()` parses a character vector.

## Usage

``` r
smvr(major = integer(), minor = 0L, patch = 0L, pre_release = "", build = "")

parse_semver(x)
```

## Arguments

- major, minor, patch:

  Non-negative integers representing the major, minor, and patch version
  components. The default values for `minor` and `patch` are `0`.

- pre_release:

  Something that can be cast to a
  [pre_release_ids](https://eitsupi.github.io/smvr/dev/reference/pre_release_ids.md)
  vector. This can be empty (`""`) meaning non pre-release (default).

- build:

  Optional build metadata character vector. Should have the pattern
  `^[a-zA-Z0-9-]+` and can contain multiple components separated by dots
  (`.`). This can be empty (`""`) meaning no build metadata (default).

- x:

  A character vector representing semantic versions. Each version should
  follow the [Semantic Versioning Specification](https://semver.org/).
  Partial matches are not allowed (e.g., `"1.0"` is not valid).

## Value

A smvr class vector.

## Details

Build metadata is not used for ordering, but the `==` and `!=` operators
check it and exactly same build metadata is required for equality. The
other operators (`<`, `<=`, `>`, `>=`) ignore build metadata.

## See also

- [`as_smvr()`](https://eitsupi.github.io/smvr/dev/reference/as_smvr.md)
  to convert other classes to smvr.

- [extract-component](https://eitsupi.github.io/smvr/dev/reference/extract-component.md)
  functions to extract components of a smvr object. (Operations opposite
  to `smvr()`).

- [update-version](https://eitsupi.github.io/smvr/dev/reference/update-version.md)
  functions to update components of a smvr object.

## Examples

``` r
# SemVer versions from components
smvr(4, 1:5)
#> <smvr[5]>
#> [1] 4.1.0 4.2.0 4.3.0 4.4.0 4.5.0

# Parse SemVer versions from character
v <- parse_semver(c(
  "1.0.0",
  "1.0.0-alpha",
  "1.0.0-beta",
  "1.0.0-rc.2",
  "1.0.0-rc.10",
  NA
))
v
#> <smvr[6]>
#> [1] 1.0.0       1.0.0-alpha 1.0.0-beta  1.0.0-rc.2  1.0.0-rc.10 <NA>       

# Sorting
vctrs::vec_sort(v)
#> <smvr[6]>
#> [1] 1.0.0-alpha 1.0.0-beta  1.0.0-rc.2  1.0.0-rc.10 1.0.0       <NA>       

# Can be compared with string notation
v[v >= "1.0.0-rc.2" & !is.na(v)]
#> <smvr[3]>
#> [1] 1.0.0       1.0.0-rc.2  1.0.0-rc.10

# Partial version components are treated as NA
suppressWarnings(parse_semver("1.5"))
#> <smvr[1]>
#> [1] <NA>

# The numeric_version class supports versions with
# less than 3 components, and can be cast to smvr.
numeric_version("1.5") |>
  vctrs::vec_cast(smvr())
#> <smvr[1]>
#> [1] 1.5.0

# Be careful with hyphens in numeric_version and SemVer.
# The following examples yield opposite results.
numeric_version("1.0.0-1") > "1.0.0" # 1.0.0-1 is the same as 1.0.0.1
#> [1] TRUE
parse_semver("1.0.0-1") > "1.0.0"    # 1.0.0-1 is a pre-release version
#> [1] FALSE
```
