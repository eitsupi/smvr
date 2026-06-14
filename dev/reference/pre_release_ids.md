# Pre-release identifiers

A class representing a collection of
[identifiers](https://eitsupi.github.io/smvr/dev/reference/pre_release_identifier.md),
which are used for representing pre-release versions.

There are two functions to create the pre_release_ids vector:

- `pre_release_ids()` is a low-level constructor for creating
  pre-release identifiers from individual components.

- `parse_pre_release_ids()` parses a character vector into pre-release
  identifiers.

## Usage

``` r
new_pre_release_ids(...)

parse_pre_release_ids(x)
```

## Arguments

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Single pre-release identifiers. Each identifier can be something to be
  cast to a
  [pre_release_identifier](https://eitsupi.github.io/smvr/dev/reference/pre_release_identifier.md)
  vector by
  [`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html).
  All components must be of the same length or length 1 (will be
  recycled).

- x:

  A character vector representing pre-release identifiers. Each
  identifier separated by a dot (`.`) will be parsed as a
  [pre_release_identifier](https://eitsupi.github.io/smvr/dev/reference/pre_release_identifier.md).

## Value

A pre_release_ids vector.

## Details

If the components are empty, they are treated as the highest precedence
pre-release ids, which is used to indicate that the version is *not a
pre-release version*.

## Limitations

There are some limitations on the number of identifiers in some
operations:

- When comparing with a string, the number of identifiers in the string.
  If it exceeds 5, an error is raised.

- When assigning, the number of identifiers in the value being assigned.
  If it exceeds the number of identifiers in the target or 5, whichever
  is larger, an error is raised.

Please refer to the examples for details.

## Examples

``` r
# Each components are concatenated with a dot
new_pre_release_ids("rc", 1:3)
#> rc.1
#> rc.2
#> rc.3

ids <- parse_pre_release_ids(
  c("", "alpha.beta", "alpha.1", "beta", "beta.11", "beta.2")
)
ids
#> <empty>
#> alpha.beta
#> alpha.1
#> beta
#> beta.11
#> beta.2

# Empty ids have the highest precedence
# (Used to indicate not a pre-release version)
vctrs::vec_sort(ids)
#> alpha.1
#> alpha.beta
#> beta
#> beta.2
#> beta.11
#> <empty>

# Can be compared with string notation
ids[ids > "beta.2"]
#> <empty>
#> beta.11

# Limitations:
# 1. When comparing with a string, the number of identifiers in the string
#    must not exceed 5.
try(ids[ids > "beta.2.3.4.5.6"])
#> Error in vec_compare(e1, e2) : 
#>   `x` and `y` are not comparable: must have the same number of columns

# This works since the string is parsed first.
ids[ids > parse_pre_release_ids("beta.2.3.4.5.6")]
#> <empty>
#> beta.11

# 2. When assigning, the number of identifiers in the value being assigned
#    must not exceed the number of identifiers in the target or 5,
#    whichever is larger.
try(ids[1] <- parse_pre_release_ids("beta.2.3.4.5.6"))
#> Error in vec_assign(vec_data(x), i, vec_data(value)) : 
#>   Can't convert from <data.frame<
#>   is_empty: logical
#>   id1     : pre_release_identifier
#>   id2     : pre_release_identifier
#>   id3     : pre_release_identifier
#>   id4     : pre_release_identifier
#>   id5     : pre_release_identifier
#>   id6     : pre_release_identifier
#> >> to <data.frame<
#>   is_empty: logical
#>   id1     : pre_release_identifier
#>   id2     : pre_release_identifier
#>   id3     : pre_release_identifier
#>   id4     : pre_release_identifier
#>   id5     : pre_release_identifier
#> >> due to loss of precision.
```
