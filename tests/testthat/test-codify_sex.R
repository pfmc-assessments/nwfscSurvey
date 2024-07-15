test_that("AFSC Slope pull biological table of Pacific ocean perch sexes", {
  skip_on_cran()

  dat <- pull_bio(
    common_name = "Pacific ocean perch",
    years = c(1910, 2020),
    survey = "AFSC.Slope"
  )
  originaltable <- table(dat[["Lengths"]][["Sex"]])
  testthat::expect_equal(
    c(2395, 3126, 600), # Values calculated on 2022-11-10 by CRW
    as.numeric(originaltable)
  )
  # Check that recoding doesn't do anything
  testthat::expect_equal(
    originaltable,
    table(codify_sex(dat[["Lengths"]][["Sex"]]))
  )
  # Check that exchanging the first two values, which are U, for NA
  # does not change the results b/c NA are coded to U
  testthat::expect_equal(
    originaltable,
    table(codify_sex(c(NA, NA, dat[["Lengths"]][["Sex"]])[-(1:2)]))
  )
})
