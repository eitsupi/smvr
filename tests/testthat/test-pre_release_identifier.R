test_that("pre_release_identifier formatting", {
  expect_equal(format(new_pre_release_identifier("alpha")), "alpha")
  expect_equal(format(new_pre_release_identifier("01")), "01")
  expect_equal(format(new_pre_release_identifier("0")), "0")
})

test_that("pre_release_identifier: comparison and ordering", {
  x <- new_pre_release_identifier(c("1", "2", "10", "a", "b", ""))
  expect_equal(
    sort(x),
    new_pre_release_identifier(c("", "1", "2", "10", "a", "b"))
  )

  expect_identical(
    new_pre_release_identifier(c("10", "1", "a")) >
      new_pre_release_identifier("2"),
    c(TRUE, FALSE, TRUE)
  )
})

test_that("pre_release_identifier: type promotion and ptype2", {
  expect_identical(
    c(new_pre_release_identifier("1"), "2"),
    c("1", "2")
  )
  expect_identical(
    c(new_pre_release_identifier("1"), 2L),
    new_pre_release_identifier(c("1", "2"))
  )
})

test_that("NA can be cast to pre_release_identifier", {
  expect_identical(
    vec_c(NA, NA, new_pre_release_identifier("1")),
    new_pre_release_identifier(c(NA, NA, "1"))
  )
})
