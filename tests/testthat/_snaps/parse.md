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
      ! Invalid version strings detected, setting to `NA`.
      x Problematic values: ""
    Output
      <smvr[1]>
      [1] <NA>

---

    Code
      parse_semver("01.2.3")
    Condition
      Warning:
      ! Invalid version strings detected, setting to `NA`.
      x Problematic values: "01.2.3"
    Output
      <smvr[1]>
      [1] <NA>

---

    Code
      parse_semver("1.02.3")
    Condition
      Warning:
      ! Invalid version strings detected, setting to `NA`.
      x Problematic values: "1.02.3"
    Output
      <smvr[1]>
      [1] <NA>

---

    Code
      parse_semver("1.2.03")
    Condition
      Warning:
      ! Invalid version strings detected, setting to `NA`.
      x Problematic values: "1.2.03"
    Output
      <smvr[1]>
      [1] <NA>

---

    Code
      parse_semver("1.2.3-alpha.01")
    Condition
      Warning:
      ! Invalid version strings detected, setting to `NA`.
      x Problematic values: "1.2.3-alpha.01"
    Output
      <smvr[1]>
      [1] <NA>

# parse_pre_release_ids rejects invalid pre-release identifiers

    Code
      parse_pre_release_ids(c("alpha..beta", "..", "--", "1.2.3.", "1.01.100"))
    Condition
      Warning:
      ! Invalid pre-release ids detected, setting to `NA`.
      x Problematic values: "alpha..beta", "..", "1.2.3.", and "1.01.100"
      i Pre-release ids must match the pattern: "^((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*)$"
    Output
      <NA>
      <NA>
      --
      <NA>
      <NA>

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

