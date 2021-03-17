test_that("Example for createStrataDF.fn works", {
  strata <- CreateStrataDF.fn(
    names          = c("deep_s", "shallow_s", "deep_mn", "shallow_m", "shallow_n"),
    depths.shallow = c(183,  55, 183, 55, 55),
    depths.deep    = c(549, 183, 549, 183, 183),
    lats.south     = c( 32,  32,  40,  40, 44),
    lats.north     = c( 40,  40,  49,  44, 49))
  expect_is(strata, "data.frame")
  expect_equal(NROW(strata), 5)
  expect_equal(ignore_attr = FALSE, tolerance = 1.1e-4,
    strata[["area"]],
    c(16468.110, 16109.711, 12478.575, 7042.491, 16390.383)
  )
  expect_equal(ignore_attr = FALSE,
    colnames(strata),
    c("name", "area",
      sprintf("%s.%d", rep(c("Depth_m", "Latitude_dd"), each = 2), 1:2))
  )
})

test_that("Naming convention of CreateStrataDF.fn works with NA", {
  strata <- CreateStrataDF.fn(
    depths.shallow = c(55, 75, 100, 125),
    depths.deep    = c(200, 250, 300, 350),
    lats.south     = c(32, 32, 40, 40),
    lats.north     = c(40, 40, 49, 44)
  )
  expect_equal(ignore_attr = FALSE,
    strata[["name"]],
    LETTERS[1:4]
  )
  expect_equal(ignore_attr = FALSE, tolerance = 1.1e-7,
    strata[["area"]],
    c(16802.988, 15537.046, 19077.597, 4559.138)
  )
  strata <- CreateStrataDF.fn(
    names          = NA,
    depths.shallow = c(55, 75, 100, 125),
    depths.deep    = c(200, 250, 300, 350),
    lats.south     = c(32, 32, 40, 40),
    lats.north     = c(40, 40, 49, 44)
  )
  expect_equal(ignore_attr = FALSE,
    strata[["name"]],
    LETTERS[1:4]
  )
})

test_that("Naming convention of CreateStrataDF.fn works with NA interspersed", {
  strata <- CreateStrataDF.fn(
    names          = c("aa", "bb", NA, "cc"),
    depths.shallow = c(55, 75, 100, 125),
    depths.deep    = c(200, 250, 300, 350),
    lats.south     = c(32, 32, 40, 40),
    lats.north     = c(40, 40, 49, 44)
  )
  expect_equal(ignore_attr = FALSE,
    strata[["name"]],
    c("aa", "bb", "A", "cc")
  )
})

test_that("CreateStrataDF.fn throws errors", {
  expect_error(CreateStrataDF.fn(
    names          = c("aa", "bb", NA, "cc"),
    depths.shallow = c(5, 75, 100, 125),
    depths.deep    = c(200, 250, 300, 350),
    lats.south     = c(32, 32, 40, 40),
    lats.north     = c(40, 40, 49, 44)
  ))
  expect_error(CreateStrataDF.fn(
    names          = c(NA, NA, "cc"),
    depths.shallow = c(50, 75, 100, 125),
    depths.deep    = c(200, 250, 300, 350),
    lats.south     = c(32, 32, 40, 40),
    lats.north     = c(40, 40, 49, 44)
  ))
  expect_error(CreateStrataDF.fn(
    depths.shallow = c(50, 75, 100, 125),
    depths.deep    = c(200, 250, 300, 350),
    lats.south     = c(32, 32, 40, 40),
    lats.north     = c(40, 40, 49, 100)
  ))
})
