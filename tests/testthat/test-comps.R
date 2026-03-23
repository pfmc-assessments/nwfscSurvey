context("Create composition data")

if (interactive()) {
  options(mc.cores = parallel::detectCores())
}
# devtools::test()
set.seed(1)

test_that("get_raw_comps", {
  skip_on_cran()

  length_comps <- get_raw_comps(
    data = bio_nwfsc_combo,
    comp_bins = seq(16, 80, 4),
    comp_column_name = "Length_cm",
    two_sex_comps = TRUE,
    input_n_method = "total_samples"
  )
  expect_equal(nrow(length_comps$sexed), 17)
  expect_equal(
    sum(length_comps$sexed$input_n),
    sum(length_comps$sexed[, 7:ncol(length_comps$sexed)])
  )
  expect_equal(nrow(length_comps$unsexed), 17)
  expect_equal(
    sum(length_comps$unsexed$input_n),
    sum(length_comps$unsexed[, 7:ncol(length_comps$unsexed)])
  )

  # confirm that plot_comps works when providing the full list
  comp_plot <- plot_comps(length_comps)
  expect_equal(is(comp_plot[[1]]), "ggplot2::ggplot")
  expect_equal(is(comp_plot[[2]]), "ggplot2::ggplot")
  # confirm that plot_comps works when providing sexed comps
  comp_plot <- plot_comps(length_comps$sexed)
  expect_equal(is(comp_plot[[1]]), "ggplot2::ggplot")
  expect_equal(is(comp_plot[[2]]), "ggplot2::ggplot")
  # confirm that plot_comps works when providing unsexed comps
  comp_plot <- plot_comps(length_comps$unsexed)
  expect_equal(is(comp_plot[[1]]), "ggplot2::ggplot")
  expect_equal(is(comp_plot[[2]]), "ggplot2::ggplot")

  length_unsexed_comps <- get_raw_comps(
    data = bio_nwfsc_combo,
    comp_bins = seq(16, 80, 4),
    comp_column_name = "Length_cm",
    two_sex_comps = FALSE,
    input_n_method = "stewart_hamel"
  )
  expect_equal(nrow(length_unsexed_comps$unsexed), 17)
  expect_equal(sum(length_unsexed_comps$unsexed$input_n), 8497)

  age_comps <- get_raw_comps(
    data = bio_nwfsc_combo,
    comp_bins = 1:12,
    comp_column_name = "Age",
    two_sex_comps = TRUE,
    input_n_method = "total_samples"
  )
  expect_equal(nrow(age_comps$sexed), 17)
  expect_equal(
    sum(age_comps$sexed$input_n),
    sum(age_comps$sexed[, 10:ncol(age_comps$sexed)])
  )
  expect_equal(nrow(age_comps$unsexed), 17)
  expect_equal(
    sum(age_comps$unsexed$input_n),
    sum(age_comps$unsexed[, 10:ncol(age_comps$unsexed)])
  )
})

test_that("get_raw_comps_triennial", {
  skip_on_cran()

  dat <- pull_bio(
    common_name = "yellowtail rockfish",
    years = c(1980, 2004),
    survey = "Triennial",
    verbose = TRUE
  )
  length_data <- dat$length_data |>
    dplyr::filter(Sex != "U")

  length_comps <- get_raw_comps(
    data = length_data,
    comp_bins = seq(20, 50, 2),
    comp_column_name = "Length_cm",
    two_sex_comps = TRUE,
    input_n_method = "total_samples"
  )
  expect_equal(nrow(length_comps$sexed), 9)
  expect_equal(
    sum(length_comps$sexed$input_n),
    sum(length_comps$sexed[, 7:ncol(length_comps$sexed)])
  )
  expect_equal(nrow(length_comps$unsexed), NULL)

  length_unsexed_comps <- get_raw_comps(
    data = length_data,
    comp_bins = seq(20, 50, 2),
    comp_column_name = "Length_cm",
    two_sex_comps = FALSE,
    input_n_method = "stewart_hamel"
  )
  expect_equal(nrow(length_unsexed_comps$unsexed), 9)
  expect_equal(sum(length_unsexed_comps$unsexed$input_n), 1125)
})


test_that("get_raw_caal", {
  skip_on_cran()

  caal <- get_raw_caal(
    data = bio_nwfsc_combo,
    len_bins = seq(16, 80, 4),
    age_bins = 1:12
  )
  expect_equal(nrow(caal), 613)
  expect_equal(ncol(caal), 33)
  expect_equal(sum(caal$input_n), sum(caal[, 10:ncol(caal)]))

  caal_unsexed <- get_raw_caal(
    data = bio_nwfsc_combo |> dplyr::filter(Sex == "U"),
    len_bins = seq(16, 80, 4),
    age_bins = 1:12
  )
  expect_equal(nrow(caal_unsexed), 69)
  expect_equal(ncol(caal_unsexed), 21)
  expect_equal(
    sum(caal_unsexed$input_n),
    sum(caal_unsexed[, 10:ncol(caal_unsexed)])
  )
})

test_that("get_expanded_comps", {
  strata <- create_strata(
    names = c(
      "shallow_wa",
      "shallow_or",
      "shallow_nca",
      "shelf_wa",
      "shelf_or",
      "shelf_nca"
    ),
    depths_shallow = c(55, 55, 55, 183, 183, 183),
    depths_deep = c(183, 183, 183, 300, 300, 300),
    lats_south = c(46.0, 42.0, 40.0, 46.0, 42.0, 40.0),
    lats_north = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0)
  )

  length_comps <- get_expanded_comps(
    bio_data = bio_nwfsc_combo,
    catch_data = catch_nwfsc_combo,
    comp_bins = seq(14, 80, 4),
    strata = strata,
    comp_column_name = "length_cm",
    two_sex_comps = TRUE,
    output = "full_expansion_ss3_format",
    input_n_method = "stewart_hamel",
    verbose = FALSE
  )
  expect_equal(nrow(length_comps$sexed), 17)
  expect_equal(sum(length_comps$sexed$input_n), 5075)
  expect_equal(nrow(length_comps$unsexed), 14)
  expect_equal(sum(length_comps$unsexed$input_n), 84)

  length_comps_sexed <- get_expanded_comps(
    bio_data = bio_nwfsc_combo |> dplyr::filter(Sex != "U"),
    catch_data = catch_nwfsc_combo,
    comp_bins = seq(14, 80, 4),
    strata = strata,
    comp_column_name = "length_cm",
    two_sex_comps = TRUE,
    output = "full_expansion_ss3_format",
    input_n_method = "stewart_hamel",
    verbose = FALSE
  )
  expect_equal(nrow(length_comps_sexed$sexed), 17)
  expect_equal(sum(length_comps_sexed$sexed$input_n), 5075)
  expect_equal(nrow(length_comps_sexed$unsexed), 0)

  # confirm that plot_comps works for expanded length comps
  comp_plot <- plot_comps(length_comps)
  expect_equal(is(comp_plot[[1]]), "ggplot2::ggplot")
  expect_equal(is(comp_plot[[1]]), "ggplot2::ggplot")

  age_comps <- get_expanded_comps(
    bio_data = bio_nwfsc_combo,
    catch_data = catch_nwfsc_combo,
    comp_bins = 0:12,
    strata = strata,
    comp_column_name = "age",
    two_sex_comps = TRUE,
    output = "full_expansion_ss3_format",
    input_n_method = "stewart_hamel"
  )
  expect_equal(nrow(age_comps$sexed), 16)
  expect_equal(sum(age_comps$sexed$input_n), 3640)
  expect_equal(nrow(age_comps$unsexed), 11)
  expect_equal(sum(age_comps$unsexed$input_n), 45)

  # confirm that plot_comps works for expanded age comps
  comp_plot <- plot_comps(age_comps)
  expect_equal(is(comp_plot[[1]]), "ggplot2::ggplot")
  expect_equal(is(comp_plot[[1]]), "ggplot2::ggplot")
})

test_that("tow_expansions", {
  strata <- create_strata(
    names = c(
      "shallow_wa",
      "shallow_or",
      "shallow_nca",
      "shelf_wa",
      "shelf_or",
      "shelf_nca"
    ),
    depths_shallow = c(55, 55, 55, 183, 183, 183),
    depths_deep = c(183, 183, 183, 300, 300, 300),
    lats_south = c(46.0, 42.0, 40.0, 46.0, 42.0, 40.0),
    lats_north = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0)
  )

  length_comps <- get_expanded_comps(
    bio_data = bio_nwfsc_combo,
    catch_data = catch_nwfsc_combo,
    comp_bins = seq(14, 80, 4),
    strata = strata,
    comp_column_name = "length_cm",
    output = "tow_expansion_only"
  )
  expect_equal(nrow(length_comps), 10439)
  expect_equal(round(sum(length_comps$exp_m), 0), 6373)
  expect_equal(round(sum(length_comps$exp_f), 0), 11841)
  expect_equal(round(sum(length_comps$exp_u), 0), 133)
})

test_that("unformatted_comps", {
  strata <- create_strata(
    names = c(
      "shallow_wa",
      "shallow_or",
      "shallow_nca",
      "shelf_wa",
      "shelf_or",
      "shelf_nca"
    ),
    depths_shallow = c(55, 55, 55, 183, 183, 183),
    depths_deep = c(183, 183, 183, 300, 300, 300),
    lats_south = c(46.0, 42.0, 40.0, 46.0, 42.0, 40.0),
    lats_north = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0)
  )

  length_comps <- get_expanded_comps(
    bio_data = bio_nwfsc_combo,
    catch_data = catch_nwfsc_combo,
    comp_bins = seq(14, 80, 4),
    strata = strata,
    comp_column_name = "length_cm",
    output = "full_expansion_unformatted"
  )
  expect_equal(nrow(length_comps), 289)
  expect_equal(round(sum(length_comps$prop_female), 0), 1090)
  expect_equal(round(sum(length_comps$prop_male), 0), 610)
  expect_equal(round(sum(length_comps$prop_unsexed), 0), 1400)
})

test_that("get_input_n", {
  n <- get_input_n(
    data = bio_nwfsc_combo,
    comp_column_name = "length_cm",
    input_n_method = "stewart_hamel",
    species_group = "other",
    verbose = TRUE
  )
  expect_equal(sum(n[n$sex_grouped == "all", "n_tows"]), 3574)

  n <- get_input_n(
    data = bio_nwfsc_combo,
    comp_column_name = "age",
    input_n_method = "stewart_hamel",
    species_group = "other",
    verbose = TRUE
  )

  expect_equal(sum(n[n$sex_grouped == "sexed", "n_tows"]), 3461)
})
