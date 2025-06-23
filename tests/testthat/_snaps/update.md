# invalid build metadata

    Code
      add_build_metadata(smvr(1:5), c("foo", "bar.baz", "@foo", ":bar"))
    Condition
      Error in `add_build_metadata()`:
      ! `metadata` must match the pattern "^[0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*$".
      x Problematic values: "@foo" and ":bar"

