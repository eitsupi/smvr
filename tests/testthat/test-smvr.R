test_that("major, minor, and patch must be non-negative", {
  expect_snapshot(smvr(-100:-1), error = TRUE)
  expect_snapshot(smvr(0L, -1:-100, 0L), error = TRUE)
  expect_snapshot(smvr(0L, 0L, -1:-100), error = TRUE)
})

test_that("smvr basic construction and formatting", {
  expect_equal(format(smvr(1L, 2L, 3L)), "1.2.3")
  expect_equal(
    format(smvr(1L, 2L, 3L, pre_release = pre_release_ids("alpha", "1"))),
    "1.2.3-alpha.1"
  )
  expect_equal(format(smvr(1L, 2L, 3L, build = "build.5")), "1.2.3+build.5")
})

test_that("smvr ordering", {
  smvr_vec <- c(
    "1.0.0+foo",
    "1.0.0",
    "1.0.0+bar",
    "2.0.0",
    "1.0.0-a",
    "1.0.0-a.1"
  ) |>
    parse_smvr()
  expect_identical(
    vec_sort(smvr_vec),
    parse_smvr(c(
      "1.0.0-a",
      "1.0.0-a.1",
      "1.0.0+foo",
      "1.0.0",
      "1.0.0+bar",
      "2.0.0"
    ))
  )
})

test_that("comparison", {
  smvr_vec <- parse_smvr(c("1.0.0", "1.0.0+build", "1.0.0-alpha.2", "2.0.0"))

  # pre-release identifiers
  expect_identical(
    smvr_vec > parse_smvr("1.0.0-alpha"),
    c(TRUE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    smvr_vec > parse_smvr("1.0.0-alpha.10"),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  expect_identical(
    smvr_vec > parse_smvr("1.0.0-beta.1"),
    c(TRUE, TRUE, FALSE, TRUE)
  )

  # build is ignored when comparing except for `==` and `!=`
  expect_identical(
    smvr_vec == parse_smvr("1.0.0"),
    c(TRUE, FALSE, FALSE, FALSE)
  )
  expect_identical(
    smvr_vec != parse_smvr("1.0.0"),
    c(FALSE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    smvr_vec <= parse_smvr("1.0.0+foobar"),
    c(TRUE, TRUE, TRUE, FALSE)
  )
  expect_identical(
    smvr_vec >= parse_smvr("1.0.0+foobar"),
    c(TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("NA can be cast to pre_release_ids", {
  expect_identical(
    vec_c(NA, NA, smvr(1L, 2L, 3L)),
    smvr(c(NA, NA, 1L), c(NA, NA, 2L), c(NA, NA, 3L))
  )
})

test_that("cast from/to character", {
  expect_identical(
    vec_cast(smvr(1L, 2L, 3L), character()),
    "1.2.3"
  )
  expect_snapshot(
    vec_cast(
      c("1.0.0", "1.0", "1.0.0+build", "1.0.0-alpha.1", "foo"),
      smvr()
    )
  )
})
