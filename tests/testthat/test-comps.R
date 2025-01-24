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
    input_n_method = "total_samples"
  )
  expect_equal(nrow(length_comps$sexed), 16)
  expect_equal(sum(length_comps$sexed$input_n), sum(length_comps$sexed[, 7:ncol(length_comps$sexed)]))
  expect_equal(nrow(length_comps$unsexed), 16)
  expect_equal(sum(length_comps$unsexed$input_n), sum(length_comps$unsexed[, 7:ncol(length_comps$unsexed)]))

  # confirm that plot_comps works when providing the full list
  comp_plot <- plot_comps(length_comps)
  expect_equal(is(comp_plot), "gg")
  # confirm that plot_comps works when providing sexed comps
  comp_plot <- plot_comps(length_comps$sexed)
  expect_equal(is(comp_plot), "gg")
  # confirm that plot_comps works when providing unsexed comps
  comp_plot <- plot_comps(length_comps$unsexed)
  expect_equal(is(comp_plot), "gg")

  length_unsexed_comps <- get_raw_comps(
    data = dat,
    comp_bins = seq(16, 80, 4),
    comp_column_name = "Length_cm",
    two_sex_comps = FALSE,
    input_n_method = "stewart_hamel"
  )
  expect_equal(nrow(length_unsexed_comps$unsexed), 16)
  expect_equal(sum(length_unsexed_comps$unsexed$input_n), 8195)

  age_comps <- get_raw_comps(
    data = dat,
    comp_bins = 1:12,
    comp_column_name = "Age",
    two_sex_comps = TRUE,
    input_n_method = "total_samples"
  )
  expect_equal(nrow(age_comps$sexed), 16)
  expect_equal(sum(age_comps$sexed$input_n), sum(age_comps$sexed[, 10:ncol(age_comps$sexed)]))
  expect_equal(nrow(age_comps$unsexed), 16)
  expect_equal(sum(age_comps$unsexed$input_n), sum(age_comps$unsexed[, 10:ncol(age_comps$unsexed)]))
})


test_that("get_raw_caal", {
  skip_on_cran()

  dat <- pull_bio(
    common_name = "lingcod",
    years = c(2003, 2018),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  caal <- get_raw_caal(
    data = dat,
    len_bins = seq(16, 80, 4),
    age_bins = 1:12
  )
  expect_equal(nrow(caal), 582)
  expect_equal(ncol(caal), 33)
  expect_equal(sum(as.numeric(caal$input_n)), 8780)
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
  strata <- CreateStrataDF.fn(
    names = c("shallow_wa", "shallow_or", "shallow_nca", "shelf_wa", "shelf_or", "shelf_nca"),
    depths.shallow = c(55, 55, 55, 183, 183, 183),
    depths.deep = c(183, 183, 183, 300, 300, 300),
    lats.south = c(46.0, 42.0, 40.0, 46.0, 42.0, 40.0),
    lats.north = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0)
  )

  length_comps <- get_expanded_comps(
    bio_data = bio,
    catch_data = catch,
    comp_bins = seq(14, 80, 4),
    strata = strata,
    comp_column_name = "length_cm",
    two_sex_comps = TRUE,
    output = "full_expansion_ss3_format",
    input_n_method = "stewart_hamel",
    verbose = FALSE
  )
  expect_equal(nrow(length_comps$sexed), 16)
  expect_equal(sum(length_comps$sexed$input_n), 4883)
  expect_equal(nrow(length_comps$unsexed), 14)
  expect_equal(sum(length_comps$unsexed$input_n), 84)

  # confirm that plot_comps works for expanded length comps
  comp_plot <- plot_comps(length_comps)
  expect_equal(is(comp_plot), "gg")

  age_comps <- get_expanded_comps(
    bio_data = bio,
    catch_data = catch,
    comp_bins = 0:12,
    strata = strata,
    comp_column_name = "age",
    two_sex_comps = TRUE,
    output = "full_expansion_ss3_format",
    input_n_method = "stewart_hamel"
  )
  expect_equal(nrow(age_comps$sexed), 15)
  expect_equal(sum(age_comps$sexed$input_n), 3541)
  expect_equal(nrow(age_comps$unsexed), 11)
  expect_equal(sum(age_comps$unsexed$input_n), 45)

  # confirm that plot_comps works for expanded age comps
  comp_plot <- plot_comps(age_comps)
  expect_equal(is(comp_plot), "gg")
})

test_that("tow_expansions", {
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
  strata <- CreateStrataDF.fn(
    names = c("shallow_wa", "shallow_or", "shallow_nca", "shelf_wa", "shelf_or", "shelf_nca"),
    depths.shallow = c(55, 55, 55, 183, 183, 183),
    depths.deep = c(183, 183, 183, 300, 300, 300),
    lats.south = c(46.0, 42.0, 40.0, 46.0, 42.0, 40.0),
    lats.north = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0)
  )

  length_comps <- get_expanded_comps(
    bio_data = bio,
    catch_data = catch,
    comp_bins = seq(14, 80, 4),
    strata = strata,
    comp_column_name = "length_cm",
    output = "tow_expansion_only"
  )
  expect_equal(nrow(length_comps), 10104)
  expect_equal(round(sum(length_comps$exp_m), 0), 6262)
  expect_equal(round(sum(length_comps$exp_f), 0), 11558)
  expect_equal(round(sum(length_comps$exp_u), 0), 133)
})

test_that("unformatted_comps", {
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
  strata <- CreateStrataDF.fn(
    names = c("shallow_wa", "shallow_or", "shallow_nca", "shelf_wa", "shelf_or", "shelf_nca"),
    depths.shallow = c(55, 55, 55, 183, 183, 183),
    depths.deep = c(183, 183, 183, 300, 300, 300),
    lats.south = c(46.0, 42.0, 40.0, 46.0, 42.0, 40.0),
    lats.north = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0)
  )

  length_comps <- get_expanded_comps(
    bio_data = bio,
    catch_data = catch,
    comp_bins = seq(14, 80, 4),
    strata = strata,
    comp_column_name = "length_cm",
    output = "full_expansion_unformatted"
  )
  expect_equal(nrow(length_comps), 272)
  expect_equal(round(sum(length_comps$prop_female), 0), 1017)
  expect_equal(round(sum(length_comps$prop_male), 0), 583)
  expect_equal(round(sum(length_comps$prop_unsexed), 0), 1400)
})

test_that("get_input_n", {
  bio <- pull_bio(
    common_name = "lingcod",
    years = c(2003, 2018),
    survey = "NWFSC.Combo",
    verbose = FALSE
  )

  n <- get_input_n(
    data = bio,
    comp_column_name = "length_cm",
    input_n_method = "stewart_hamel",
    species_group = "other",
    verbose = TRUE
  )
  expect_equal(sum(n[n$sex_grouped == "all", "n_tows"]), 3447)

  n <- get_input_n(
    data = bio,
    comp_column_name = "age",
    input_n_method = "stewart_hamel",
    species_group = "other",
    verbose = TRUE
  )

  expect_equal(sum(n[n$sex_grouped == "sexed", "n_tows"]), 3335)
})
