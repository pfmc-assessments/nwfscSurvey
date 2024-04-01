context("Data processing")

if (interactive()) options(mc.cores = parallel::detectCores())
# devtools::test()
set.seed(1)

test_that("pull_catch", {
  skip_on_cran()

  set.seed(123)
  dat <- pull_catch(
    common_name = "lingcod",
    years = c(2003, 2018),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 10351)
})

test_that("pull_catch-multispecies", {
  skip_on_cran()

  set.seed(123)
  dat <- pull_catch(
    years = 2017,
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 350126)
  expect_equal(length(which(dat$cpue_kg_km2 == 0)), 330971)

  dat_lingcod <- pull_catch(
    common_name = "lingcod",
    years = c(2017),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  dat_lingcod_sablefish <- pull_catch(
    common_name = c("lingcod", "sablefish"),
    years = c(2017),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  expect_equal(
    NROW(dplyr::filter(dat, Common_name == "lingcod")),
    NROW(dat_lingcod),
    label = "entries of all species filtered for lingcod",
    expected.label = "entries of lingcod"
  )
  expect_equal(
    NROW(dplyr::filter(dat_lingcod_sablefish, Common_name == "lingcod")),
    NROW(dat_lingcod),
    label = "entries of 2 species filtered for lingcod",
    expected.label = "entries of lingcod"
  )
})

test_that("PullHaul", {
  skip_on_cran()

  set.seed(123)
  dat <- PullHaul.fn(
    YearRange = c(2003, 2018),
    SurveyName = "NWFSC.Combo",
    Dir = NULL, verbose = TRUE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 10351)
})

test_that("pull_bio", {
  skip_on_cran()

  set.seed(123)
  dat <- pull_bio(
    common_name = "lingcod",
    years = c(2016, 2017),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 3363)
})

test_that("pull_bio_triennial", {
  skip_on_cran()

  set.seed(123)
  dat <- pull_bio(
    common_name = "lingcod",
    years = c(1980, 1992),
    survey = "Triennial",
    verbose = TRUE
  )
  expect_is(dat, "list")
  expect_equal(nrow(dat[[1]]), 1596)
  expect_equal(nrow(dat[[2]]), 382)
})

test_that("pull_biological_samples", {
  skip_on_cran()

  set.seed(123)
  dat <- pull_biological_samples(
    common_name = "lingcod",
    years = c(2003, 2017),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 926)
})
