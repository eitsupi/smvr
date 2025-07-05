# numeric values can be cast to pre_release_identifier

    Code
      vec_cast(-2:2, new_pre_release_identifier())
    Condition
      Error in `vec_cast.pre_release_identifier.integer()`:
      ! Cannot cast negative integer to <pre_release_identifier>.
      x Problematic values: -2 and -1

---

    Code
      vec_cast(c(1.2, 1), new_pre_release_identifier())
    Condition
      Error:
      ! Can't convert from `x` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      vec_cast(c(-1, 1), new_pre_release_identifier())
    Condition
      Error in `vec_cast.pre_release_identifier.integer()`:
      ! Cannot cast negative integer to <pre_release_identifier>.
      x Problematic values: -1

# print(<pre_release_identifier>) works

    Code
      new_pre_release_identifier(c("", "-1", "0", "00", "1", "Foo", "bar", NA))
    Output
      <empty>
      -1 <alphanumeric>
      0 <numeric>
      00 <alphanumeric>
      1 <numeric>
      Foo <alphanumeric>
      bar <alphanumeric>
      <NA>

