# pre_release_ids construction and empty rules

    Code
      new_pre_release_ids("alpha", "", c("", "foo", "bar", ""), c("", "0", "", "0"))
    Condition
      Error in `new_pre_release_ids()`:
      ! All ids after the first empty must also be empty.
      x Problematic indices: 2, 3, and 4

# print

    Code
      new_pre_release_ids()
    Output
      new_pre_release_ids()

---

    Code
      new_pre_release_ids("alpha", "1")
    Output
      alpha.1

---

    Code
      new_pre_release_ids(c("", "alpha"), c("", "1"))
    Output
      <empty>
      alpha.1

# pre_release_ids comparison

    Code
      new_pre_release_ids("0") > "1.2.3.4.5.6"
    Condition
      Error in `vec_compare()`:
      ! `x` and `y` are not comparable: must have the same number of columns

---

    Code
      new_pre_release_ids(!!!1:6) == "1.2.3.4.5.6.7"
    Condition
      Error:
      ! `x` and `y` must have the same number of columns

# cast from/to character

    Code
      vec_cast(c("alpha.1", "beta.2", "rc.3", NA), new_pre_release_ids())
    Output
      alpha.1
      beta.2
      rc.3
      <NA>

# assigning

    Code
      ids <- new_pre_release_ids(c("foo", "bar"))
      ids[1] <- new_pre_release_ids("baz", "1")
      vec_data(ids)
    Output
        is_empty id1 id2 id3 id4 id5
      1    FALSE baz   1            
      2    FALSE bar                

---

    Code
      ids <- new_pre_release_ids(c("foo", "bar"))
      ids[1] <- "baz.1"
      vec_data(ids)
    Output
        is_empty id1 id2 id3 id4 id5
      1    FALSE baz   1            
      2    FALSE bar                

---

    Code
      ids <- new_pre_release_ids(c("foo", "bar"))
      ids[1] <- parse_pre_release_ids("baz.1.2.3.4.5")
    Condition
      Error in `vec_assign()`:
      ! Can't convert from <data.frame<
        is_empty: logical
        id1     : pre_release_identifier
        id2     : pre_release_identifier
        id3     : pre_release_identifier
        id4     : pre_release_identifier
        id5     : pre_release_identifier
        id6     : pre_release_identifier
      >> to <data.frame<
        is_empty: logical
        id1     : pre_release_identifier
        id2     : pre_release_identifier
        id3     : pre_release_identifier
        id4     : pre_release_identifier
        id5     : pre_release_identifier
      >> due to loss of precision.

