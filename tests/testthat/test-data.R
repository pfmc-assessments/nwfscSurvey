context("Data processing")

if (interactive()) {
  options(mc.cores = parallel::detectCores())
}
# devtools::test()
set.seed(1)

test_that("pull_catch", {
  skip_on_cran()

  dat <- pull_catch(
    common_name = "lingcod",
    years = c(2003, 2018),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 10358)
})

test_that("pull_catch-multispecies", {
  skip_on_cran()

  dat <- pull_catch(
    years = 2017,
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 395792)
  expect_equal(length(which(dat$cpue_kg_km2 == 0)), 376542)

  dat_lingcod <- pull_catch(
    common_name = "lingcod",
    years = c(2017),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  dat_lingcod_sablefish <- pull_catch(
    common_name = c("lingcod", "sablefish"),
    years = c(2017),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  expect_equal(
    NROW(dplyr::filter(dat, Common_name == "lingcod")),
    NROW(dat_lingcod),
    label = "entries of all species filtered for lingcod",
    expected.label = "entries of lingcod"
  )
  expect_equal(
    NROW(dplyr::filter(dat_lingcod_sablefish, Common_name == "lingcod")),
    NROW(dat_lingcod),
    label = "entries of 2 species filtered for lingcod",
    expected.label = "entries of lingcod"
  )
})

test_that("pull-sample-types", {
  skip_on_cran()

  data_hake <- pull_catch(
    common_name = "Pacific hake",
    years = c(2014, 2018),
    survey = "NWFSC.Combo",
    verbose = TRUE,
    convert = TRUE,
    sample_types = c("NA", NA, "Life Stage", "Size")
  )
  expect_is(data_hake, "data.frame")
  expect_equal(nrow(data_hake), 3559)
  expect_equal(length(which(data_hake$cpue_kg_km2 == 0)), 1625)
  expect_equal(length(unique(data_hake$Trawl_id)), 3445)

  combine_hake <- combine_tows(
    data = data_hake
  )
  expect_equal(length(unique(data_hake$Trawl_id)), nrow(combine_hake))
  expect_equal(
    sum(data_hake$total_catch_numbers),
    sum(combine_hake$total_catch_numbers)
  )

  data_hake_3_types <- pull_catch(
    common_name = "Pacific hake",
    years = c(2014, 2018),
    survey = "NWFSC.Combo",
    verbose = TRUE,
    convert = TRUE,
    sample_types = c("NA", NA, "Life Stage", "Size")[1:3]
  )
  expect_equal(
    sum(table(data_hake[
      which(data_hake$Partition_sample_types != "Size"),
      "Partition_sample_types"
    ])),
    sum(table(data_hake_3_types[, "Partition_sample_types"]))
  )

  data_eggs <- pull_catch(
    common_name = "big skate",
    years = c(2014, 2019),
    survey = "NWFSC.Combo",
    verbose = TRUE,
    convert = TRUE,
    sample_types = c("NA", NA, "Life Stage", "Size")
  )

  combine_eggs <- combine_tows(
    data = data_eggs
  )
  expect_equal(
    nrow(data_eggs) - sum(data_eggs$Partition %in% c("Eggs", "Egg Cases")),
    nrow(combine_eggs)
  )
  expect_equal(
    sum(data_eggs$total_catch_numbers[which(
      !data_eggs$Partition %in% c("Eggs", "Egg Cases")
    )]),
    sum(combine_eggs$total_catch_numbers)
  )
})

test_that("pull_catch_unfiltered", {
  skip_on_cran()

  dat <- pull_catch(
    common_name = "lingcod",
    years = c(2003, 2018),
    survey = "NWFSC.Combo",
    dir = NULL,
    verbose = FALSE,
    standard_filtering = FALSE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 11302)
})

test_that("pull_bio", {
  skip_on_cran()

  dat <- pull_bio(
    common_name = "lingcod",
    years = c(2016, 2017),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 3379)
})

test_that("pull_catch_triennial", {
  skip_on_cran()

  dat <- pull_catch(
    common_name = "Pacific ocean perch",
    survey = "Triennial",
    verbose = TRUE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 4457)
})

test_that("pull_bio_triennial", {
  skip_on_cran()

  dat <- pull_bio(
    common_name = "lingcod",
    years = c(1980, 1992),
    survey = "Triennial",
    verbose = TRUE
  )
  expect_is(dat, "list")
  expect_equal(nrow(dat[[1]]), 1596)
  expect_equal(nrow(dat[[2]]), 382)

  dat <- pull_bio(
    common_name = "quillback rockfish",
    years = c(1980, 2004),
    survey = "Triennial",
    verbose = TRUE
  )
  expect_is(dat, "list")
  expect_equal(nrow(dat[[1]]), 48)
  expect_equal(dat[[2]], "no_ages_available")
})


test_that("pull_catch_nwfsc_slope", {
  skip_on_cran()

  dat <- pull_catch(
    common_name = "sablefish",
    survey = "NWFSC.Slope",
    verbose = TRUE
  ) |>
    dplyr::mutate(
      positive = dplyr::case_when(total_catch_numbers > 0 ~ 1, .default = 0)
    )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 1714)
  expect_equal(sum(dat[, "positive"]), 1481)
})

test_that("pull_bio_nwfsc_slope", {
  skip_on_cran()

  dat <- pull_bio(
    common_name = "sablefish",
    survey = "NWFSC.Slope",
    verbose = TRUE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 15336)
})

test_that("pull_catch_afsc_slope", {
  skip_on_cran()

  dat <- pull_catch(
    common_name = "sablefish",
    survey = "AFSC.Slope",
    verbose = TRUE
  ) |>
    dplyr::mutate(
      positive = dplyr::case_when(total_catch_numbers > 0 ~ 1, .default = 0)
    )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 1696)
  expect_equal(sum(dat[, "positive"]), 1671)
})

test_that("pull_bio_afsc_slope", {
  skip_on_cran()

  dat <- pull_bio(
    common_name = "sablefish",
    survey = "AFSC.Slope",
    verbose = TRUE
  )
  expect_is(dat, "list")
  expect_equal(nrow(dat[[1]]), 47269)
  expect_equal(nrow(dat[[2]]), 14763)
})


test_that("pull_biological_samples", {
  skip_on_cran()

  dat <- pull_biological_samples(
    common_name = "lingcod",
    years = c(2003, 2017),
    survey = "NWFSC.Combo",
    verbose = TRUE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 1132)
})

test_that("pull_gemm", {
  skip_on_cran()

  dat <- pull_gemm(
    common_name = "lingcod",
    years = c(2005, 2010)
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 64)
})

test_that("pull_haul_nwfsc_combo", {
  skip_on_cran()

  dat <- pull_haul(
    years = c(2003, 2018),
    survey = "NWFSC.Combo",
    dir = NULL,
    verbose = FALSE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 10358)
})

test_that("pull_haul_triennial", {
  skip_on_cran()

  dat <- pull_haul(
    survey = "Triennial",
    dir = NULL,
    verbose = FALSE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 4457)
})

test_that("pull_haul_afsc_slope", {
  skip_on_cran()

  dat <- pull_haul(
    survey = "AFSC.Slope",
    verbose = FALSE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 1696)
})

test_that("pull_haul_nwfsc_slope", {
  skip_on_cran()

  dat <- pull_haul(
    survey = "NWFSC.Slope",
    verbose = FALSE
  )
  expect_is(dat, "data.frame")
  expect_equal(nrow(dat), 1714)
})
