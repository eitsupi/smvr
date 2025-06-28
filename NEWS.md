# smvr (development version)

## Breaking changes

- `new_pre_release_ids()`'s arguments have been changed from `id1, id2, id3, id4, id5` to `...`.
  This allows for more than 5 pre-release identifiers to be specified. (#8)

## Enhancements

- `parse_semver()`, `parse_pre_release_ids()`, and `new_pre_release_ids()` have been rewritten
  to significantly improve string processing performance. (#15)

# smvr 0.1.0

Initial release.
