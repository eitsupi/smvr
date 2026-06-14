# Changelog

## smvr (development version)

## smvr 0.2.2

CRAN release: 2025-10-13

### New features

- `SEM_VER_PATTERN`, which is a suggested regular expression (RegEx) to
  check a SemVer string
  ([\#33](https://github.com/eitsupi/smvr/issues/33)).

## smvr 0.2.1

CRAN release: 2025-08-19

### Bug fixes

- Numeric only pre-release identifiers starting with 0 (e.g., `01`) are
  no longer allowed, and use the correct regular expression pattern from
  the SemVer 2.0.0 documentation
  ([\#31](https://github.com/eitsupi/smvr/issues/31)).

## smvr 0.2.0

CRAN release: 2025-07-05

### Breaking changes

- [`new_pre_release_ids()`](https://eitsupi.github.io/smvr/dev/reference/pre_release_ids.md)’s
  arguments have been changed from `id1, id2, id3, id4, id5` to `...`.
  This allows for more than 5 pre-release identifiers to be specified.
  ([\#8](https://github.com/eitsupi/smvr/issues/8))
- [`mark_as_pre_release()`](https://eitsupi.github.io/smvr/dev/reference/update-version.md)’s
  `ids` argument no longer has a default value.
  ([\#23](https://github.com/eitsupi/smvr/issues/23))
- The [`smvr()`](https://eitsupi.github.io/smvr/dev/reference/smvr.md)
  returns `NA` with a warning instead of an error when an invalid value
  is passed to the `build` argument.
  ([\#26](https://github.com/eitsupi/smvr/issues/26))

### Enhancements

- [`parse_semver()`](https://eitsupi.github.io/smvr/dev/reference/smvr.md),
  [`parse_pre_release_ids()`](https://eitsupi.github.io/smvr/dev/reference/pre_release_ids.md),
  and
  [`new_pre_release_ids()`](https://eitsupi.github.io/smvr/dev/reference/pre_release_ids.md)
  have been rewritten to significantly improve string processing
  performance. ([\#15](https://github.com/eitsupi/smvr/issues/15))

### Bug fixes

- Negative numbers can no longer be cast to `pre_release_identifier` and
  treated as alphanumeric values.
  ([\#16](https://github.com/eitsupi/smvr/issues/16))

## smvr 0.1.0

CRAN release: 2025-06-27

Initial release.
