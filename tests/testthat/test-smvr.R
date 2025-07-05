test_that("major, minor, and patch must be non-negative", {
  expect_snapshot(smvr(-100:-1), error = TRUE)
  expect_snapshot(smvr(0L, -1:-100, 0L), error = TRUE)
  expect_snapshot(smvr(0L, 0L, -1:-100), error = TRUE)
})

test_that("smvr basic construction and formatting", {
  expect_equal(format(smvr(1L, 2L, 3L)), "1.2.3")
  expect_equal(
    format(smvr(1L, 2L, 3L, pre_release = new_pre_release_ids("alpha", "1"))),
    "1.2.3-alpha.1"
  )
  expect_equal(format(smvr(1L, 2L, 3L, build = "build.5")), "1.2.3+build.5")
})

test_that("smvr ordering", {
  expect_equal(
    rank(parse_semver(
      c(
        "1.0.0+foo",
        "1.0.0",
        "1.0.0+bar",
        "2.0.0",
        "1.0.0-a",
        "1.0.0-a.1"
      )
    )),
    c(3, 4, 5, 6, 1, 2)
  )

  expect_equal(
    rank(parse_semver(
      c(
        "1.0.0-alpha",
        "1.0.0-alpha.1",
        "1.0.0-alpha.beta",
        "1.0.0-beta",
        "1.0.0-beta.2",
        "1.0.0-beta.11",
        "1.0.0-rc.1",
        "1.0.0"
      )
    )),
    1:8
  )
})

test_that("comparison", {
  smvr_vec <- parse_semver(c("1.0.0", "1.0.0+build", "1.0.0-alpha.2", "2.0.0"))

  # pre-release identifiers
  expect_identical(
    smvr_vec > parse_semver("1.0.0-alpha"),
    c(TRUE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    smvr_vec > parse_semver("1.0.0-alpha.10"),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  expect_identical(
    smvr_vec > parse_semver("1.0.0-beta.1"),
    c(TRUE, TRUE, FALSE, TRUE)
  )

  # build is ignored when comparing except for `==` and `!=`
  expect_identical(
    smvr_vec == parse_semver("1.0.0"),
    c(TRUE, FALSE, FALSE, FALSE)
  )
  expect_identical(
    smvr_vec != parse_semver("1.0.0"),
    c(FALSE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    smvr_vec <= parse_semver("1.0.0+foobar"),
    c(TRUE, TRUE, TRUE, FALSE)
  )
  expect_identical(
    smvr_vec >= parse_semver("1.0.0+foobar"),
    c(TRUE, TRUE, FALSE, TRUE)
  )

  # Can be compared with string notation
  expect_identical(
    smvr_vec >= "1.0.0-alpha.10",
    c(TRUE, TRUE, FALSE, TRUE)
  )

  # When the string notation have more than 5 components,
  # the comparison will fail
  expect_snapshot(
    smvr_vec >= "0.0.0-1.2.3.4.5.6",
    error = TRUE
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

test_that("cast from/to numeric_version", {
  expect_identical(
    vec_cast(parse_semver(c("1.2.3", NA)), numeric_version(character())),
    numeric_version(c("1.2.3", NA), strict = FALSE)
  )
  expect_snapshot(
    vec_cast(
      parse_semver(c("1.0.0", "1.0.0+build", "1.0.0-a.1", "1.0.0-a.2")),
      numeric_version(character())
    ),
    error = TRUE
  )

  expect_identical(
    vec_cast(
      numeric_version(c("1", "1.2", "1.2.3", NA), strict = FALSE),
      smvr()
    ),
    smvr(c(1L, 1L, 1L, NA), c(0L, 2L, 2L, NA), c(0L, 0L, 3L, NA))
  )
  expect_snapshot(
    vec_cast(
      numeric_version(c(NA, "1.2.3", "1.2.3.4", "1.2.3.4-5"), strict = FALSE),
      smvr()
    ),
    error = TRUE
  )
})

test_that("invalid build metadata", {
  expect_snapshot(
    smvr(1, build = c("foo", "bar.baz", NA, "@foo"))
  )
})
