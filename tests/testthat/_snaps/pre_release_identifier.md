# print(<>pre_release_identifier>) works

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

