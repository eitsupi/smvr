test_that("pre_release_ids construction and empty rules", {
  expect_snapshot(
    new_pre_release_ids(
      "alpha",
      "",
      c("", "foo", "bar", ""),
      c("", "0", "", "0")
    ),
    error = TRUE
  )
})

test_that("format.pre_release_ids works", {
  x <- new_pre_release_ids("alpha", "1")
  expect_equal(format(x), "alpha.1")
  y <- new_pre_release_ids("alpha")
  expect_equal(format(y), "alpha")
})

test_that("print", {
  expect_snapshot(new_pre_release_ids())
  expect_snapshot(new_pre_release_ids("alpha", "1"))
  expect_snapshot(new_pre_release_ids(c("", "alpha"), c("", "1")))
})

test_that("pre_release_ids comparison", {
  expect_true(
    new_pre_release_ids("alpha", "1") > new_pre_release_ids("alpha", "0")
  )
  expect_true(
    new_pre_release_ids("alpha", "1") == new_pre_release_ids("alpha", "1")
  )
  expect_true(
    new_pre_release_ids("alpha", "1") > new_pre_release_ids("alpha")
  )
  expect_true(
    new_pre_release_ids("alpha", "foo") > new_pre_release_ids("alpha")
  )
  expect_true(
    new_pre_release_ids("alpha", "10") > new_pre_release_ids("alpha", "2")
  )
  # The identifier_type attribute is consistent across all values,
  # so when comparing through the vector, the result sometimes unexpectedly
  expect_identical(
    # alphanumeric
    new_pre_release_ids(c("10", "1", "a")) > new_pre_release_ids("2"),
    c(TRUE, FALSE, TRUE)
  )
  expect_identical(
    # numeric
    new_pre_release_ids(c("10", "1", "0")) > new_pre_release_ids("2"),
    c(TRUE, FALSE, FALSE)
  )

  # Can be compared with string notation
  expect_identical(
    new_pre_release_ids(c("alpha", "beta", "rc"), c("", "10", "")) > "beta.2",
    c(FALSE, TRUE, TRUE)
  )

  # When comparing with string notation,
  # if the string has many components,
  # the comparison will fail
  expect_snapshot(
    new_pre_release_ids("0") > "1.2.3.4.5.6",
    error = TRUE
  )
  # The same number of components is allowed
  expect_true(new_pre_release_ids(!!!1:6) == "1.2.3.4.5.6")
  expect_snapshot(
    new_pre_release_ids(!!!1:6) == "1.2.3.4.5.6.7",
    error = TRUE
  )
})

test_that("NA can be cast to pre_release_ids", {
  expect_identical(
    vec_c(
      NA,
      NA,
      new_pre_release_ids(
        new_pre_release_identifier("rc"),
        new_pre_release_identifier("1")
      )
    ),
    new_pre_release_ids(
      vec_c(NA, NA, new_pre_release_identifier("rc")),
      vec_c("", "", new_pre_release_identifier("1"))
    )
  )
})

test_that("cast from/to character", {
  expect_identical(
    vec_cast(
      new_pre_release_ids(c("alpha", "beta", "rc", NA), "1"),
      character()
    ),
    c("alpha.1", "beta.1", "rc.1", NA)
  )
  expect_snapshot(
    vec_cast(
      c("alpha.1", "beta.2", "rc.3", NA),
      new_pre_release_ids()
    )
  )
})

test_that("assigning", {
  expect_snapshot({
    ids <- new_pre_release_ids(c("foo", "bar"))
    ids[1] <- new_pre_release_ids("baz", "1")
    vec_data(ids)
  })
  expect_snapshot({
    ids <- new_pre_release_ids(c("foo", "bar"))
    ids[1] <- "baz.1"
    vec_data(ids)
  })
  # Assigning a ids having many components will fail
  expect_snapshot(
    {
      ids <- new_pre_release_ids(c("foo", "bar"))
      ids[1] <- parse_pre_release_ids("baz.1.2.3.4.5")
    },
    error = TRUE
  )
})
