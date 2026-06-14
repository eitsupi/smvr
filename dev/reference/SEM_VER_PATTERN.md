# A suggested regular expression (RegEx) to check a SemVer string

This is a suggested regular expression (RegEx) to check a SemVer string,
without the `"^"` at the start and `"$"` at the end. It is useful to
extract the SemVer components from VCS tags etc.

## Usage

``` r
SEM_VER_PATTERN
```

## Format

An object of class `character` of length 1.

## Value

Single string

## Examples

``` r
SEM_VER_PATTERN
#> [1] "(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?"

# VCS tag names often have a "v" prefix to SemVer
tag_names <- c("v1.0.0", "v1.1.0-alpha.1", "v1.1.0+build.1", "not-a-version")

# Extract and parse SemVer
regmatches(tag_names, m = regexpr(SEM_VER_PATTERN, tag_names)) |>
  parse_semver()
#> <smvr[3]>
#> [1] 1.0.0         1.1.0-alpha.1 1.1.0+build.1
```
