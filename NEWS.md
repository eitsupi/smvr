# smvr 0.2.1

## Bug fixes

- Numeric only pre-release identifiers starting with 0 (e.g., `01`) are no longer allowed,
  and use the correct regular expression pattern from the SemVer 2.0.0 documentation (#31).

# smvr 0.2.0

## Breaking changes

- `new_pre_release_ids()`'s arguments have been changed from `id1, id2, id3, id4, id5` to `...`.
  This allows for more than 5 pre-release identifiers to be specified. (#8)
- `mark_as_pre_release()`'s `ids` argument no longer has a default value. (#23)
- The `smvr()` returns `NA` with a warning instead of an error when an invalid value
  is passed to the `build` argument. (#26)

## Enhancements

- `parse_semver()`, `parse_pre_release_ids()`, and `new_pre_release_ids()` have been rewritten
  to significantly improve string processing performance. (#15)

## Bug fixes

- Negative numbers can no longer be cast to `pre_release_identifier` and treated as alphanumeric values. (#16)

# smvr 0.1.0

Initial release.
