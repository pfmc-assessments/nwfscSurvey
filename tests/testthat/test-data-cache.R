test_that("pull_catch_cache", {
  skip_on_cran()
  data <- pull_catch_cache(
    common_name = "petrale sole",
    years = c(2008, 2016)
  )
  expect_is(data, "data.frame")
  expect_equal(nrow(data), 5969)

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
  expect_equal(nrow(data), 40853)

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
  expect_equal(nrow(data), 5969)

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
