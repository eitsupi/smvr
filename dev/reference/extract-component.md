# Extract each component of version numbers/labels

These functions extract the individual components of version numbers or
labels, such as major, minor, patch numbers, or, pre-release identifiers
and build metadata.

## Usage

``` r
extract_major(x, ...)

extract_minor(x, ...)

extract_patch(x, ...)

extract_pre_release_ids(x, ...)

extract_build_metadata(x, ...)

# S3 method for class 'smvr'
extract_major(x, ...)

# S3 method for class 'smvr'
extract_minor(x, ...)

# S3 method for class 'smvr'
extract_patch(x, ...)

# S3 method for class 'smvr'
extract_pre_release_ids(x, ...)

# S3 method for class 'smvr'
extract_build_metadata(x, ...)

# S3 method for class 'numeric_version'
extract_major(x, ...)

# S3 method for class 'numeric_version'
extract_minor(x, ...)

# S3 method for class 'numeric_version'
extract_patch(x, ...)
```

## Arguments

- x:

  A version object.

- ...:

  Additional arguments passed to methods.

## Value

The extracted component of the version object.

- `extract_major()`, `extract_minor()`, and `extract_patch()` return
  integer.

- `extract_pre_release_ids()` returns
  [pre_release_ids](https://eitsupi.github.io/smvr/dev/reference/pre_release_ids.md).

- `extract_build_metadata()` returns character.

## See also

- [`smvr()`](https://eitsupi.github.io/smvr/dev/reference/smvr.md) to
  create a [smvr](https://eitsupi.github.io/smvr/dev/reference/smvr.md)
  object from components.

- [check-component](https://eitsupi.github.io/smvr/dev/reference/check-component.md)
  functions for checking if
  [smvr](https://eitsupi.github.io/smvr/dev/reference/smvr.md) object
  has a specific component.

## Examples

``` r
sem_ver <- parse_semver(c("1.2.3-alpha+001", "2.0.0", NA))

extract_major(sem_ver)
#> [1]  1  2 NA
extract_minor(sem_ver)
#> [1]  2  0 NA
extract_patch(sem_ver)
#> [1]  3  0 NA
extract_pre_release_ids(sem_ver)
#> alpha
#> <empty>
#> <NA>
extract_build_metadata(sem_ver)
#> [1] "001" ""    NA   

# Extracting version also works for numeric_version
num_ver <- numeric_version(c("1", "3.1.4.1.5", NA), strict = FALSE)

extract_major(num_ver)
#> [1]  1  3 NA
extract_minor(num_ver)
#> [1]  0  1 NA
extract_patch(num_ver)
#> [1]  0  4 NA
```
