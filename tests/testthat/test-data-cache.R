test_that("pull_catch_cache", {
  skip_on_cran()
  data <- pull_catch_cache(
    common_name = "petrale sole",
    years = c(2008, 2016)
  )
  expect_is(data, "data.frame")
  expect_equal(nrow(data), 6005)

  api_data <- pull_catch(
    common_name = "petrale sole",
    years = c(2008, 2016)
  )
  expect_equal(nrow(data), nrow(api_data))
})

test_that("pull_bio_cache", {
  skip_on_cran()

  data <- pull_bio_cache(
    common_name = "petrale sole",
    years = c(2008, 2016)
  )
  expect_is(data, "data.frame")
  expect_equal(nrow(data), 41070)

  api_data <- pull_bio(
    common_name = "petrale sole",
    years = c(2008, 2016)
  )
  expect_equal(nrow(data), nrow(api_data))
})

test_that("pull_haul_cache", {
  skip_on_cran()
  data <- pull_haul_cache(
    years = c(2008, 2016)
  )
  expect_is(data, "data.frame")
  expect_equal(nrow(data), 6005)

  api_data <- pull_haul(
    years = c(2008, 2016)
  )
  expect_equal(nrow(data), nrow(api_data))
})

test_that("pull_catch_cache_triennial", {
  skip_on_cran()
  data <- pull_catch_cache(
    survey = "Triennial",
    common_name = "petrale sole",
    years = c(1980, 2004)
  )
  expect_is(data, "data.frame")
  expect_equal(nrow(data), 3882)

  api_data <- pull_catch(
    survey = "Triennial",
    common_name = "petrale sole",
    years = c(1980, 2004)
  )
  expect_equal(nrow(data), nrow(api_data))
})

test_that("pull_bio_cache_triennial", {
  skip_on_cran()

  data <- pull_bio_cache(
    survey = "Triennial",
    common_name = "petrale sole",
    years = c(1980, 2004)
  )
  expect_is(data, "list")
  expect_equal(nrow(data[[1]]), 14705)
  expect_equal(nrow(data[[2]]), 537)

  api_data <- pull_bio(
    survey = "Triennial",
    common_name = "petrale sole",
    years = c(1980, 2004)
  )
  expect_equal(nrow(data[[1]]), nrow(api_data[[1]]))
  expect_equal(nrow(data[[2]]), nrow(api_data[[2]]))
})

test_that("pull_haul_cache_triennial", {
  skip_on_cran()
  data <- pull_haul_cache(
    survey = "Triennial",
    years = c(1980, 2004)
  )
  expect_is(data, "data.frame")
  expect_equal(nrow(data), 3882)

  api_data <- pull_haul(
    survey = "Triennial",
    years = c(1980, 2004)
  )
  expect_equal(nrow(data), nrow(api_data))
})

test_that("pull_hkl_cache_bocaccio", {
  skip_on_cran()
  data <- pull_hkl_cache(
    common_name = "Bocaccio",
    years = c(2004, 2023)
  )
  expect_is(data, "list")
  expect_equal(nrow(data$catch_data), 200471)
  expect_equal(nrow(data$bio_data), 20394)
})

test_that("pull_hkl_cache_all_species", {
  skip_on_cran()
  data <- pull_hkl_cache(
    years = c(2004, 2023)
  )
  expect_is(data, "list")
  expect_equal(nrow(data$catch_data), 204499)
  expect_equal(nrow(data$bio_data), 90554)
})
