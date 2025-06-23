# pre_release_ids construction and empty rules

    Code
      new_pre_release_ids("alpha", "", "1", "", "")
    Condition
      Error in `new_pre_release_ids()`:
      ! All ids after the first empty must also be empty.
      x Problematic index: 1

# cast from/to character

    Code
      vec_cast(c("alpha.1", "beta.2", "rc.3", NA), new_pre_release_ids())
    Output
      alpha.1
      beta.2
      rc.3
      <NA>

