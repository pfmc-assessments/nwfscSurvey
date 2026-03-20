context("Create design based indices")

if (interactive()) {
  options(mc.cores = parallel::detectCores())
}


test_that("get_design_based_index", {
  skip_on_cran()

  dat <- pull_catch(
    common_name = "lingcod",
    years = c(2003, 2018),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
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
    data = dat,
    strata = strata
  )

  expect_equal(length(biomass), 2)
  expect_equal(nrow(biomass$biomass), 16)
  expect_equal(round(biomass$biomass$est[1], 0), 38889)
  expect_equal(nrow(biomass$biomass_by_strata), 48)
  expect_equal(biomass$biomass_by_strata$ntows[1], 142)
  expect_equal(round(biomass$biomass_by_strata$area[1], 0), 25793)
})

test_that("get_design_based_index with create_strata", {
  skip_on_cran()

  dat <- pull_catch(
    common_name = "lingcod",
    years = c(2003, 2018),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
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
    data = dat,
    strata = strata
  )

  expect_equal(length(biomass), 2)
  expect_equal(nrow(biomass$biomass), 16)
  expect_equal(round(biomass$biomass$est[1], 0), 38889)
  expect_equal(nrow(biomass$biomass_by_strata), 48)
  expect_equal(biomass$biomass_by_strata$ntows[1], 142)
  expect_equal(round(biomass$biomass_by_strata$area[1], 0), 25793)

  p <- plot_index(data = biomass)
  expect_equal(is(p[[1]]), "ggplot2::ggplot")
  expect_equal(is(p[[2]]), "ggplot2::ggplot")
})
