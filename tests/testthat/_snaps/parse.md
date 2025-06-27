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

# Supports more than 5 pre-release identifiers

    Code
      parse_semver("1.2.3-alpha.1.2.3.4.5.6.7.8.9")
    Output
      <smvr[1]>
      [1] 1.2.3-alpha.1.2.3.4.5.6.7.8.9

---

    Code
      parse_pre_release_ids("alpha.1.2.3.4.5.6.7.8.9")
    Output
      alpha.1.2.3.4.5.6.7.8.9

