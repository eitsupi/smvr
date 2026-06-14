# Single pre-release identifier

A class representing a single pre-release identifier (alphanumeric or
numeric) for Semantic Versioning 2.0.0.

## Usage

``` r
new_pre_release_identifier(x = character())
```

## Arguments

- x:

  Something that can be coerced to a character vector by
  [`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html).
  Each element must be ASCII alphanumerics, hyphens, or empty string
  (`""`). Empty string is a special case that means no identifier.

## Value

A pre_release_identifier vector.

## Details

Identifiers are compared based on the following criteria:

- If the identifier is empty, it is treated as the smallest value.

- Integers greater than or equal to 0 are treated as numeric identifiers
  and compared numerically.

- Else, identifiers are treated as alphanumeric identifiers and compared
  lexically ASCII sort order.

- Numeric identifiers always have lower precedence than alphanumeric
  identifiers.

## See also

- [pre_release_ids](https://eitsupi.github.io/smvr/dev/reference/pre_release_ids.md):
  Whole pre-release identifiers (Concatenation of
  pre_release_identifier).

## Examples

``` r
id <- new_pre_release_identifier(
  c("1", "2", "10", "0a", "-1", "alpha", "beta", "", NA)
)
id
#> 1 <numeric>
#> 2 <numeric>
#> 10 <numeric>
#> 0a <alphanumeric>
#> -1 <alphanumeric>
#> alpha <alphanumeric>
#> beta <alphanumeric>
#> <empty>
#> <NA>

# empty < numeric < alphanumeric
vctrs::vec_sort(id)
#> <empty>
#> 1 <numeric>
#> 2 <numeric>
#> 10 <numeric>
#> -1 <alphanumeric>
#> 0a <alphanumeric>
#> alpha <alphanumeric>
#> beta <alphanumeric>
#> <NA>

# Works with base R vectors.
id[id == "alpha" & !is.na(id)]
#> alpha <alphanumeric>
id[id > 2L & !is.na(id)]
#> 10 <numeric>
#> 0a <alphanumeric>
#> -1 <alphanumeric>
#> alpha <alphanumeric>
#> beta <alphanumeric>
```
