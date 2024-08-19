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
