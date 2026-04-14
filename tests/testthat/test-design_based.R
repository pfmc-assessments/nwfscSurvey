context("Create design based indices")

if (interactive()) {
  options(mc.cores = parallel::detectCores())
}

test_that("check-strata", {
  skip_on_cran()

  strata <- CreateStrataDF.fn(
    names = c("wa", "or", "ca"),
    depths.shallow = c(55, 55, 55),
    depths.deep = c(350, 350, 350),
    lats.south = c(46.0, 42.0, 32.0),
    lats.north = c(49.0, 46.0, 42.0)
  )

  n_strata <- check_strata(
    data = catch_nwfsc_combo,
    strata = strata,
    dir = NULL,
    verbose = FALSE
  )
  expect_equal(dim(n_strata)[1], 51)
  expect_equal(sum(n_strata[["tows"]]), 5958)
  expect_equal(sum(n_strata[["positive_tows"]]), 3560)
})

test_that("get_design_based_index", {
  skip_on_cran()

  strata <- CreateStrataDF.fn(
    names = c("wa", "or", "ca"),
    depths.shallow = c(55, 55, 55),
    depths.deep = c(350, 350, 350),
    lats.south = c(46.0, 42.0, 32.0),
    lats.north = c(49.0, 46.0, 42.0)
  )

  expect_equal(nrow(strata), 3)
  expect_equal(sum(round(strata$area, 0)), 52067)

  biomass <- get_design_based(
    data = catch_nwfsc_combo,
    strata = strata,
    verbose = FALSE
  )
  expect_equal(length(biomass), 2)
  expect_equal(nrow(biomass$biomass), 17)
  expect_equal(biomass$biomass$est[1], 38888.94, tolerance = 1e-3)
  expect_equal(nrow(biomass$biomass_by_strata), 51)
  expect_equal(biomass$biomass_by_strata$ntows[1], 142)
  expect_equal(biomass$biomass_by_strata$area[1], 25792.54, tolerance = 1e-3)
})

test_that("get_design_based_index with create_strata", {
  skip_on_cran()

  strata <- create_strata(
    names = c("wa", "or", "ca"),
    depths_shallow = c(55, 55, 55),
    depths_deep = c(350, 350, 350),
    lats_south = c(46.0, 42.0, 32.0),
    lats_north = c(49.0, 46.0, 42.0)
  )

  expect_equal(nrow(strata), 3)
  expect_equal(sum(round(strata$area, 0)), 52067)

  biomass <- get_design_based(
    data = catch_nwfsc_combo,
    strata = strata
  )

  expect_equal(length(biomass), 2)
  expect_equal(nrow(biomass$biomass), 17)
  expect_equal(round(biomass$biomass$est[1], 0), 38889)
  expect_equal(nrow(biomass$biomass_by_strata), 51)
  expect_equal(biomass$biomass_by_strata$ntows[1], 142)
  expect_equal(round(biomass$biomass_by_strata$area[1], 0), 25793)

  p <- plot_index(data = biomass)
  expect_equal(is(p[[1]]), "ggplot2::ggplot")
  expect_equal(is(p[[2]]), "ggplot2::ggplot")
})
