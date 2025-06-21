test_that("parse_semver parses valid semver strings", {
  x <- parse_semver(c(
    "1.2.3",
    "1.2.3-alpha.1",
    "2.0.0+build.5",
    "1.2.3-alpha+abcde",
    NA
  ))
  expect_snapshot(x)
})

test_that("parse_pre_release_ids parses valid pre-release identifiers", {
  x <- parse_pre_release_ids(c("alpha.1.2", "beta.2.3.4", "", NA))
  expect_snapshot(x)
})

test_that("parse_semver rejects invalid semver strings", {
  expect_snapshot(parse_semver(""))
  expect_snapshot(parse_semver("01.2.3"))
  expect_snapshot(parse_semver("1.02.3"))
  expect_snapshot(parse_semver("1.2.03"))
})

test_that("Only supports up to 5 pre-release identifiers", {
  expect_snapshot(parse_semver("1.2.3-alpha.1.2.3.4"))
  expect_snapshot(parse_pre_release_ids("alpha.1.2.3.4"))
  expect_snapshot(parse_semver("1.2.3-alpha.1.2.3.4.5"), error = TRUE)
  expect_snapshot(parse_pre_release_ids("alpha.1.2.3.4.5"), error = TRUE)
})
