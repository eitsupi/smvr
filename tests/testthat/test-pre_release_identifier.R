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

test_that("digits starting with 0 should not be allowed", {
  expect_snapshot(
    new_pre_release_identifier(c("0", "01", "10")),
    error = TRUE
  )
})

test_that("pre_release_identifier: type promotion and ptype2", {
  expect_identical(
    vec_c(new_pre_release_identifier("1"), "2"),
    new_pre_release_identifier(c("1", "2"))
  )
  expect_identical(
    vec_c(new_pre_release_identifier("1"), 2L),
    new_pre_release_identifier(c("1", "2"))
  )
})

test_that("NA can be cast to pre_release_identifier", {
  expect_identical(
    vec_c(NA, NA, new_pre_release_identifier("1")),
    new_pre_release_identifier(c(NA, NA, "1"))
  )
})

test_that("numeric values can be cast to pre_release_identifier", {
  expect_identical(
    vec_cast(0:3, new_pre_release_identifier()),
    new_pre_release_identifier(as.character(0:3))
  )
  expect_identical(
    vec_cast(as.double(0:3), new_pre_release_identifier()),
    new_pre_release_identifier(as.character(0:3))
  )

  expect_snapshot(vec_cast(-2:2, new_pre_release_identifier()), error = TRUE)
  expect_snapshot(
    vec_cast(c(1.2, 1), new_pre_release_identifier()),
    error = TRUE
  )
  expect_snapshot(
    vec_cast(c(-1, 1), new_pre_release_identifier()),
    error = TRUE
  )
})

test_that("print(<pre_release_identifier>) works", {
  expect_snapshot(
    new_pre_release_identifier(
      c("", "-1", "0", "1", "Foo", "bar", NA)
    )
  )
})
