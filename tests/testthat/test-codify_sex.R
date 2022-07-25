
test_that("AFSC Slope pull biological table of lingcod sexes", {
  skip_on_cran()

  dat <- PullBio.fn(
    Name = "lingcod",
    SciName = NULL, YearRange = c(1910, 2020),
    SurveyName = "AFSC.Slope", SaveFile = FALSE,
    Dir = NULL, verbose = TRUE
  )
  originaltable <- table(dat[["Lengths"]][["Sex"]])
  testthat::expect_equal(
    c(182, 35, 105), # Values calculated on 2022-06-08 by KFJ
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
