context("Data processing")

if (interactive()) options(mc.cores = parallel::detectCores())

set.seed(1)

test_that("PullCatch", {
  skip_on_cran()

  set.seed(123)
  dat = PullCatch.fn(Name = "lingcod",
    SciName = NULL, YearRange = c(2003, 2018),
    SurveyName = "NWFSC.Combo", SaveFile = FALSE,
    Dir = NULL, verbose = TRUE)
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 10365)

})

test_that("PullCatch-multispecies", {
  skip_on_cran()

  set.seed(123)
  #dat = PullCatch.fn(SciName = NULL, YearRange = 2017,
  #  SurveyName = "NWFSC.Combo", SaveFile = FALSE,
  #  Dir = NULL, verbose = TRUE)
  #expect_is(dat, "data.frame")
  #expect_equal(nrow(dat), 39109)

})

test_that("PullHaul", {
  skip_on_cran()

  set.seed(123)
  dat = PullHaul.fn(YearRange = c(2003, 2018),
    SurveyName = "NWFSC.Combo", SaveFile = FALSE,
    Dir = NULL, verbose = TRUE)
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 10375)

})

test_that("PullBio", {
  skip_on_cran()

  set.seed(123)
  dat = PullBio.fn(Name = "lingcod",
    SciName = NULL, YearRange = c(2016, 2017),
    SurveyName = "NWFSC.Combo", SaveFile = FALSE,
    Dir = NULL, verbose = TRUE)
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 3378)

})
