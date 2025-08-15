# major, minor, and patch must be non-negative

    Code
      smvr(-100:-1)
    Condition
      Error in `smvr()`:
      ! `major` must be non-negative integer values.
      x Problematic values: -100, -99, -98, -97, -96, -95, -94, -93, -92, -91, -90, -89, -88, -87, -86, -85, -84, -83, ..., -2, and -1

---

    Code
      smvr(0L, -1:-100, 0L)
    Condition
      Error in `smvr()`:
      ! `minor` must be non-negative integer values.
      x Problematic values: -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15, -16, -17, -18, ..., -99, and -100

---

    Code
      smvr(0L, 0L, -1:-100)
    Condition
      Error in `smvr()`:
      ! `patch` must be non-negative integer values.
      x Problematic values: -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15, -16, -17, -18, ..., -99, and -100

# comparison

    Code
      smvr_vec >= "0.0.0-1.2.3.4.5.6"
    Condition
      Error in `vec_compare()`:
      ! `x` and `y` are not comparable: must have the same number of columns

# cast from/to character

    Code
      vec_cast(c("1.0.0", "1.0", "1.0.0+build", "1.0.0-alpha.1", "foo"), smvr())
    Condition
      Warning:
      ! Invalid version strings detected, setting to `NA`.
      x Problematic values: "1.0" and "foo"
    Output
      <smvr[5]>
      [1] 1.0.0         <NA>          1.0.0+build   1.0.0-alpha.1 <NA>         

# cast from/to numeric_version

    Code
      vec_cast(parse_semver(c("1.0.0", "1.0.0+build", "1.0.0-a.1", "1.0.0-a.2")),
      numeric_version(character()))
    Condition
      Error in `vec_cast.numeric_version.smvr()`:
      ! Pre-release <smvr> cannot be cast to <numeric_version>.
      x Problematic values: 1.0.0-a.1 and 1.0.0-a.2

---

    Code
      vec_cast(numeric_version(c(NA, "1.2.3", "1.2.3.4", "1.2.3.4-5"), strict = FALSE),
      smvr())
    Condition
      Error in `vec_cast.smvr.numeric_version()`:
      ! Cannot cast <numeric_version> with more than 3 components to <smvr>.
      x Problematic values: 1.2.3.4 and 1.2.3.4.5

# invalid build metadata

    Code
      smvr(1, build = c("foo", "bar.baz", NA, "@foo"))
    Condition
      Warning in `smvr()`:
      ! Invalid build metadata detected, setting to `NA`.
      x Problematic values: "@foo"
      i Build metadata should have the pattern: "^([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*)$"
    Output
      <smvr[4]>
      [1] 1.0.0+foo     1.0.0+bar.baz <NA>          <NA>         

