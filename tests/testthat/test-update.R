test_that("update functions work", {
  smvr_vec <- parse_semver(
    c("1.2.3", "1.2.3-a.1", "2.0.0+foo.bar")
  )
  expect_equal(
    increment_major(smvr_vec),
    smvr(c(2, 2, 3))
  )
  expect_equal(
    increment_minor(smvr_vec),
    smvr(c(1, 1, 2), c(3, 3, 1))
  )
  expect_equal(
    increment_patch(smvr_vec),
    smvr(c(1, 1, 2), c(2, 2, 0), c(4, 4, 1))
  )
  expect_equal(
    mark_as_pre_release(smvr_vec, "beta"),
    smvr(
      c(1, 1, 2),
      c(2, 2, 0),
      c(3, 3, 0),
      "beta",
      c("", "", "foo.bar")
    )
  )
  expect_equal(
    add_build_metadata(smvr_vec, "build.1"),
    smvr(
      c(1, 1, 2),
      c(2, 2, 0),
      c(3, 3, 0),
      c("", "a.1", ""),
      "build.1"
    )
  )
  expect_equal(
    increment_major(smvr_vec) |>
      mark_as_pre_release(c("alpha", "beta", "rc")) |>
      add_build_metadata(c("a", "b", "c")),
    smvr(
      c(2, 2, 3),
      pre_release = c("alpha", "beta", "rc"),
      build = c("a", "b", "c")
    )
  )
})

test_that("invalid build metadata", {
  expect_snapshot(
    smvr(1:5) |>
      add_build_metadata(c("foo", "bar.baz", "@foo", ":bar")),
    error = TRUE
  )
})
