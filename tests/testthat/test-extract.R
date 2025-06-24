test_that("extract can extract components from smvr", {
  sem_ver <- parse_semver(c(
    "1.2.3",
    "1.2.3-alpha.1",
    "2.0.0+build.5",
    "1.2.3-alpha+abcde",
    NA
  ))

  major <- extract_major(sem_ver)
  minor <- extract_minor(sem_ver)
  patch <- extract_patch(sem_ver)
  pre_release <- extract_pre_release_ids(sem_ver)
  build <- extract_build_metadata(sem_ver)

  expect_identical(major, c(1L, 1L, 2L, 1L, NA_integer_))
  expect_identical(minor, c(2L, 2L, 0L, 2L, NA_integer_))
  expect_identical(patch, c(3L, 3L, 0L, 3L, NA_integer_))
  expect_identical(
    pre_release,
    parse_pre_release_ids(c("", "alpha.1", "", "alpha", NA_character_))
  )
  expect_identical(build, c("", "", "build.5", "abcde", NA_character_))
})
