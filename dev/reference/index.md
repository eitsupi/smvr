# Package index

## Vector classes

Constructor functions and parser functions for the vector classes
represent the SemVer versioning scheme.

### SemVer

The `smvr` class represents versions.

- [`smvr()`](https://eitsupi.github.io/smvr/dev/reference/smvr.md)
  [`parse_semver()`](https://eitsupi.github.io/smvr/dev/reference/smvr.md)
  : A vector representing versions following Semantic Versioning

### Pre-release identifier components

`pre_release_ids` and `pre_release_identifier` are used to represent
pre-release identifiers in versions.

- [`new_pre_release_ids()`](https://eitsupi.github.io/smvr/dev/reference/pre_release_ids.md)
  [`parse_pre_release_ids()`](https://eitsupi.github.io/smvr/dev/reference/pre_release_ids.md)
  : Pre-release identifiers
- [`new_pre_release_identifier()`](https://eitsupi.github.io/smvr/dev/reference/pre_release_identifier.md)
  : Single pre-release identifier

## Manipulation

- [`as_smvr()`](https://eitsupi.github.io/smvr/dev/reference/as_smvr.md)
  :

  Convert to `smvr` vector

- [`is_smvr()`](https://eitsupi.github.io/smvr/dev/reference/is_smvr.md)
  :

  Check if an object is a `smvr` object

- [`is_pre_release()`](https://eitsupi.github.io/smvr/dev/reference/check-component.md)
  [`has_build_metadata()`](https://eitsupi.github.io/smvr/dev/reference/check-component.md)
  :

  Check if the `smvr` object has a specific component

- [`extract_major()`](https://eitsupi.github.io/smvr/dev/reference/extract-component.md)
  [`extract_minor()`](https://eitsupi.github.io/smvr/dev/reference/extract-component.md)
  [`extract_patch()`](https://eitsupi.github.io/smvr/dev/reference/extract-component.md)
  [`extract_pre_release_ids()`](https://eitsupi.github.io/smvr/dev/reference/extract-component.md)
  [`extract_build_metadata()`](https://eitsupi.github.io/smvr/dev/reference/extract-component.md)
  : Extract each component of version numbers/labels

- [`increment_major()`](https://eitsupi.github.io/smvr/dev/reference/update-version.md)
  [`increment_minor()`](https://eitsupi.github.io/smvr/dev/reference/update-version.md)
  [`increment_patch()`](https://eitsupi.github.io/smvr/dev/reference/update-version.md)
  [`mark_as_pre_release()`](https://eitsupi.github.io/smvr/dev/reference/update-version.md)
  [`add_build_metadata()`](https://eitsupi.github.io/smvr/dev/reference/update-version.md)
  : Update version components

## Miscellaneous

- [`SEM_VER_PATTERN`](https://eitsupi.github.io/smvr/dev/reference/SEM_VER_PATTERN.md)
  : A suggested regular expression (RegEx) to check a SemVer string
