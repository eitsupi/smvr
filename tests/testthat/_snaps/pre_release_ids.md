# pre_release_ids construction and empty rules

    Code
      pre_release_ids("alpha", "", "1", "", "")
    Condition
      Error in `pre_release_ids()`:
      ! All ids after the first empty must also be empty.
      x Problematic index: 1

# cast from/to character

    Code
      vec_cast(c("alpha.1", "beta.2", "rc.3", NA), pre_release_ids())
    Output
      <pre_release_ids[4]>
      [1] alpha.1 beta.2  rc.3    <NA>   

