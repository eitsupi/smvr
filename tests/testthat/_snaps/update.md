# invalid build metadata

    Code
      add_build_metadata(smvr(1:5), c("foo", "bar.baz", "@foo", ":bar"))
    Condition
      Error in `add_build_metadata()`:
      ! Adding build metadata failed.
      x Invalid metadata: "@foo" and ":bar"
      i Invalid pattern or `NA` values are not allowed.

