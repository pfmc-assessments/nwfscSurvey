context("Test estimate_weight_length()")

if (interactive()) {
    options(mc.cores = parallel::detectCores())
}

test_that("estimate_weight_length", {
    skip_on_cran()

    dat <- pull_bio(
        common_name = "lingcod",
        years = c(2016, 2017),
        survey = "NWFSC.Combo",
        verbose = TRUE
    )

    wl <- estimate_weight_length(dat)

    expect_is(wl, "data.frame")
    expect_equal(wl[wl$sex == "all", "B"], 3.261785, tolerance = 1e-4)
})

# could add tests for est_growth() and est_vbgrowth() here in the future
