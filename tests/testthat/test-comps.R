context("Create composition data")

if (interactive()) options(mc.cores = parallel::detectCores())
# devtools::test()
set.seed(1)

test_that("get_raw_comps", {
  skip_on_cran()

  dat <- pull_bio(
    common_name = "lingcod",
    years = c(2003, 2018),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  length_comps <- get_raw_comps(
    data = dat,
    comp_bins = seq(16, 80, 4),
    comp_column_name = "Length_cm",
    two_sex_comps = TRUE,
    input_sample_size_method = c("stewart_hamel", "tows", "total_samples")[3]
  )
  expect_equal(nrow(length_comps$sexed), 16)
  expect_equal(sum(length_comps$sexed$input_n), sum(length_comps$sexed[, 7:ncol(length_comps$sexed)]))
  expect_equal(nrow(length_comps$unsexed), 16)
  expect_equal(sum(length_comps$unsexed$input_n), sum(length_comps$unsexed[, 7:ncol(length_comps$unsexed)]))

  length_unsexed_comps <- get_raw_comps(
    data = dat,
    comp_bins = seq(16, 80, 4),
    comp_column_name = "Length_cm",
    two_sex_comps = FALSE,
    input_sample_size_method = c("stewart_hamel", "tows", "total_samples")[3]
  )
  expect_equal(nrow(length_unsexed_comps$unsexed), 16)
  expect_equal(sum(length_unsexed_comps$unsexed$input_n), sum(length_unsexed_comps$unsexed[, 7:ncol(length_unsexed_comps$unsexed)]))

  age_comps <- get_raw_comps(
    data = dat,
    comp_bins = 1:12,
    comp_column_name = "Age",
    two_sex_comps = TRUE,
    input_sample_size_method = c("stewart_hamel", "tows", "total_samples")[3]
  )
  expect_equal(nrow(age_comps$sexed), 16)
  expect_equal(sum(age_comps$sexed$input_n), sum(age_comps$sexed[, 10:ncol(age_comps$sexed)]))
  expect_equal(nrow(age_comps$unsexed), 16)
  expect_equal(sum(age_comps$unsexed$input_n), sum(age_comps$unsexed[, 10:ncol(age_comps$unsexed)]))
})

test_that("get_expanded_comps", {

  catch <- pull_catch(
    common_name = "lingcod",
    years = c(2003, 2018),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  bio <- pull_bio(
    common_name = "lingcod",
    years = c(2003, 2018),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  strata <-  CreateStrataDF.fn(
    names = c("shallow_wa", "shallow_or", "shallow_nca", "shelf_wa", "shelf_or", "shelf_nca"),
    depths.shallow = c( 55,   55,   55,  183,  183, 183),
    depths.deep    = c(183,  183,  183,  300,  300, 300),
    lats.south     = c(46.0, 42.0, 40.0, 46.0, 42.0, 40.0),
    lats.north     = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0))

  length_comps <- get_expanded_comps(
    bio_data = bio,
    catch_data = catch,
    comp_bins = seq(14, 80, 4),
    strata = strata,
    comp_column_name = "length_cm",
    two_sex_comps = TRUE,
    input_sample_size_method = "stewart_hamel",
    verbose = FALSE
  )
  expect_equal(nrow(length_comps$sexed), 16)
  expect_equal(sum(length_comps$sexed$input_n), 7928)
  expect_equal(nrow(length_comps$unsexed), 14)
  expect_equal(sum(length_comps$unsexed$input_n), 587)

  age_comps <- get_expanded_comps(
    bio_data = bio,
    catch_data = catch,
    comp_bins = 0:12,
    strata = strata,
    comp_column_name = "age",
    two_sex_comps = TRUE,
    input_sample_size_method = "stewart_hamel"
   )
  expect_equal(nrow(age_comps$sexed), 15)
  expect_equal(sum(age_comps$sexed$input_n), 3541)
  expect_equal(nrow(age_comps$unsexed), 11)
  expect_equal(sum(age_comps$unsexed$input_n), 45)

})