test_that("extract can extract components from numeric_version", {
  num_ver <- numeric_version(
    c(
      "1",
      "1.2",
      "1.2.3",
      "1.2.3.4",
      NA
    ),
    strict = FALSE
  )

  major <- extract_major(num_ver)
  minor <- extract_minor(num_ver)
  patch <- extract_patch(num_ver)

  expect_identical(major, c(1L, 1L, 1L, 1L, NA_integer_))
  expect_identical(minor, c(0L, 2L, 2L, 2L, NA_integer_))
  expect_identical(patch, c(0L, 0L, 3L, 3L, NA_integer_))
})
