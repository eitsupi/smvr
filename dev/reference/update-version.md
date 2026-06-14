# Update version components

These functions allows to update the components of version objects.

- `increment_major()`, `increment_minor()`, and `increment_patch()`
  update the major, minor, and patch version numbers respectively. Note
  that these functions reset the pre-release and build metadata to
  empty.

- `mark_as_pre_release()` marks the version as a pre-release version.

- `add_build_metadata()` adds build metadata to the version.

## Usage

``` r
increment_major(x, ...)

# S3 method for class 'smvr'
increment_major(x, ...)

increment_minor(x, ...)

# S3 method for class 'smvr'
increment_minor(x, ...)

increment_patch(x, ...)

# S3 method for class 'smvr'
increment_patch(x, ...)

mark_as_pre_release(x, ...)

# S3 method for class 'smvr'
mark_as_pre_release(x, ids, ...)

add_build_metadata(x, ...)

# S3 method for class 'smvr'
add_build_metadata(x, metadata = "", ...)
```

## Arguments

- x:

  An version object

- ...:

  Additional arguments passed to methods.

- ids:

  Something can be cast to
  [pre_release_ids](https://eitsupi.github.io/smvr/dev/reference/pre_release_ids.md)
  representing the pre-release identifiers, length must be 1 or the same
  as `x`.

- metadata:

  A character vector of build metadata, length must be 1 or the same as
  `x`.

## Value

An updated version object with the specified changes applied.

## Examples

``` r
v <- parse_semver(c("0.9.9", "1.0.0-a.1", "1.1.0+1"))

increment_major(v)
#> <smvr[3]>
#> [1] 1.0.0 2.0.0 2.0.0
increment_minor(v)
#> <smvr[3]>
#> [1] 0.10.0 1.1.0  1.2.0 
increment_patch(v)
#> <smvr[3]>
#> [1] 0.9.10 1.0.1  1.1.1 
mark_as_pre_release(v, ids = "rc.1")
#> <smvr[3]>
#> [1] 0.9.9-rc.1   1.0.0-rc.1   1.1.0-rc.1+1
add_build_metadata(v, metadata = "build.1")
#> <smvr[3]>
#> [1] 0.9.9+build.1     1.0.0-a.1+build.1 1.1.0+build.1    
```
