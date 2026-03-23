context("Test estimate_weight_length()")

if (interactive()) {
  options(mc.cores = parallel::detectCores())
}

test_that("estimate_weight_length", {
  skip_on_cran()

  wl <- estimate_weight_length(bio_nwfsc_combo)

  expect_is(wl, "data.frame")
  expect_equal(wl[wl$sex == "all", "B"], 3.309217, tolerance = 1e-3)
})

# could add tests for est_growth() and est_vbgrowth() here in the future

test_that("estimate_growth", {
  growth <- est_growth(
    dat = bio_nwfsc_combo
  )
  expect_equal(nrow(growth), 25256)
  expect_equal(ncol(growth), 33)

  growth <- est_growth(
    dat = bio_nwfsc_combo,
    return_df = FALSE
  )
  expect_equal(length(growth), 6)
  expect_equal(growth$female_growth[["K"]], 0.16595254, tolerance = 1e-3)
})

test_that("estimate_vbgrowth", {
  growth <- est_vbgrowth(
    data = bio_nwfsc_combo
  )
  expect_equal(length(growth), 6)
  expect_equal(growth$female_growth[["K"]], 0.16595254, tolerance = 1e-3)
})
