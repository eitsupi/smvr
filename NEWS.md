# smvr (development version)

## Breaking changes

- `new_pre_release_ids()`'s arguments have been changed from `id1, id2, id3, id4, id5` to `...`.
  This allows for more than 5 pre-release identifiers to be specified. (#8)
- `mark_as_pre_release()`'s `ids` argument no longer has a default value. (#23)

## Enhancements

- `parse_semver()`, `parse_pre_release_ids()`, and `new_pre_release_ids()` have been rewritten
  to significantly improve string processing performance. (#15)

## Bug fixes

- Negative numbers can no longer be cast to `pre_release_identifier` and treated as alphanumeric values. (#16)

# smvr 0.1.0

Initial release.
