test_that("plot_age_length_sampling", {
  p <- plot_age_length_sampling(
    data = bio_nwfsc_combo
  )
  expect_equal(is(p[[1]]), "ggplot2::ggplot")
  expect_equal(is(p[[2]]), "ggplot2::ggplot")
})

test_that("plot_cpue", {
  p <- plot_cpue(
    data = catch_nwfsc_combo
  )
  expect_equal(is(p[[1]]), "ggplot2::ggplot")
  expect_equal(is(p[[2]]), "ggplot2::ggplot")
  expect_equal(is(p[[3]]), "ggplot2::ggplot")
})

test_that("plot_cpue_map", {
  p <- plot_cpue_map(
    data = catch_nwfsc_combo
  )
  expect_equal(is(p[[1]]), "ggplot2::ggplot")
  expect_equal(is(p[[2]]), "ggplot2::ggplot")
})

test_that("plot_length_age", {
  p <- plot_length_age(
    data = bio_nwfsc_combo
  )
  expect_equal(is(p), "ggplot2::ggplot")
})

test_that("plot_proportions", {
  temp <- catch_nwfsc_combo |>
    dplyr::mutate(
      new = factor(
        cpue_kg_km2 <= 0,
        levels = c(FALSE, TRUE),
        labels = c("Present", "Absent")
      )
    )

  p <- plot_proportion(
    data = temp,
    column_factor = new,
    column_bin = Depth_m,
    width = 50,
    boundary = 0
  )
  expect_equal(is(p), "ggplot2::ggplot")
})

test_that("plot_sex_ratio", {
  p <- plot_sex_ratio(
    data = bio_nwfsc_combo,
    comp_column_name = "age"
  )
  expect_equal(is(p), "ggplot2::ggplot")
  p <- plot_sex_ratio(
    data = bio_nwfsc_combo,
    comp_column_name = "length_cm"
  )
  expect_equal(is(p), "ggplot2::ggplot")
})

test_that("plot_sex_ratio_strata", {
  strata <- create_strata(
    names = c("wa", "or", "ca"),
    depths_shallow = c(55, 55, 55),
    depths_deep = c(350, 350, 350),
    lats_south = c(46.0, 42.0, 32.0),
    lats_north = c(49.0, 46.0, 42.0)
  )
  p <- plot_sex_ratio_strata(
    data = bio_nwfsc_combo,
    strata_df = strata
  )
  expect_equal(is(p), "ggplot2::ggplot")
})

test_that("plot_var_length_at_age", {
  p <- plot_var_length_at_age(
    data = bio_nwfsc_combo,
    age_bins = 1:10
  )
  expect_equal(is(p), "ggplot2::ggplot")
})

test_that("plot_weight_length", {
  p <- plot_weight_length(data = bio_nwfsc_combo)
  expect_equal(is(p), "ggplot2::ggplot")
})

test_that("plot_bio_patterns", {
  p <- plot_bio_patterns(
    data = bio_nwfsc_combo |>
      dplyr::filter(Sex %in% c("F", "M"), Year %in% 2003:2005),
    col_name = "Age"
  )
  expect_equal(is(p[[1]]), "ggplot2::ggplot")
  expect_equal(is(p[[2]]), "ggplot2::ggplot")
  expect_equal(is(p[[3]]), "ggplot2::ggplot")
})
