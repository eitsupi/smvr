# parse_semver parses valid semver strings

    Code
      x
    Output
      <smvr[5]>
      [1] 1.2.3             1.2.3-alpha.1     2.0.0+build.5     1.2.3-alpha+abcde
      [5] <NA>             

# parse_pre_release_ids parses valid pre-release identifiers

    Code
      x
    Output
      alpha.1.2
      beta.2.3.4
      <empty>
      <NA>

# parse_semver rejects invalid semver strings

    Code
      parse_semver("")
    Condition
      Warning:
      ! Cannot parse '' as semantic version, setting to `NA`.
    Output
      <smvr[1]>
      [1] <NA>

---

    Code
      parse_semver("01.2.3")
    Condition
      Warning:
      ! Cannot parse '01.2.3' as semantic version, setting to `NA`.
    Output
      <smvr[1]>
      [1] <NA>

---

    Code
      parse_semver("1.02.3")
    Condition
      Warning:
      ! Cannot parse '1.02.3' as semantic version, setting to `NA`.
    Output
      <smvr[1]>
      [1] <NA>

---

    Code
      parse_semver("1.2.03")
    Condition
      Warning:
      ! Cannot parse '1.2.03' as semantic version, setting to `NA`.
    Output
      <smvr[1]>
      [1] <NA>

# Only supports up to 5 pre-release identifiers

    Code
      parse_semver("1.2.3-alpha.1.2.3.4")
    Output
      <smvr[1]>
      [1] 1.2.3-alpha.1.2.3.4

---

    Code
      parse_pre_release_ids("alpha.1.2.3.4")
    Output
      alpha.1.2.3.4

---

    Code
      parse_semver("1.2.3-alpha.1.2.3.4.5")
    Condition
      Error in `parse_semver()`:
      ! Unsupported pre-release identifiers in '1.2.3-alpha.1.2.3.4.5'.
      ! Only up to 5 pre-release identifiers are supported, got 6.

---

    Code
      parse_pre_release_ids("alpha.1.2.3.4.5")
    Condition
      Error in `parse_pre_release_ids()`:
      ! Unsupported pre-release identifiers in 'alpha.1.2.3.4.5'.
      ! Only up to 5 pre-release identifiers are supported, got 6.

