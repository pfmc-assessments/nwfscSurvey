test_that("petrale_sole_wcgbt", {
  catch_data <- pull_catch(
    common_name = "petrale sole",
    years = c(2003, 2025)
  )
  bio_data <- pull_bio(
    common_name = "petrale sole",
    years = c(2003, 2025)
  )

  expect_equal(nrow(catch_data), 14179)
  expect_equal(sum(catch_data$total_catch_wt_kg > 0), 6108)
  expect_equal(sum(catch_data$cpue_kg_km2), 7007254)

  expect_equal(nrow(bio_data), 87590)
  expect_equal(sum(bio_data$Length_cm, na.rm = TRUE), 3057571, tolerance = 0.5)
})

test_that("petrale_sole_triennial", {
  catch_data <- pull_catch(
    common_name = "petrale sole",
    survey = "Triennial"
  )
  bio_data <- pull_bio(
    common_name = "petrale sole",
    survey = "Triennial"
  )

  expect_equal(nrow(catch_data), 4457)
  expect_equal(sum(catch_data$total_catch_wt_kg > 0), 2347)
  expect_equal(sum(catch_data$cpue_kg_km2), 282216.7, tolerance = 0.5)

  expect_equal(nrow(bio_data$length_data), 14705)
  expect_equal(sum(bio_data$length_data$Length_cm, na.rm = TRUE), 469066)
  expect_equal(nrow(bio_data$age_data), 537)
})

test_that("shortspine_wcgbt", {
  catch_data <- pull_catch(
    common_name = "shortspine thornyhead",
    years = c(2003, 2025)
  )
  bio_data <- pull_bio(
    common_name = "shortspine thornyhead",
    years = c(2003, 2025)
  )

  expect_equal(nrow(catch_data), 14179)
  expect_equal(sum(catch_data$total_catch_wt_kg > 0), 7203)
  expect_equal(sum(catch_data$cpue_kg_km2), 5619109)

  expect_equal(nrow(bio_data), 108478)
  expect_equal(sum(bio_data$Length_cm, na.rm = TRUE), 3212723)
})

test_that("shortspine_triennial", {
  catch_data <- pull_catch(
    common_name = "shortspine thornyhead",
    survey = "Triennial"
  )
  bio_data <- pull_bio(
    common_name = "shortspine thornyhead",
    survey = "Triennial"
  )

  expect_equal(nrow(catch_data), 4457)
  expect_equal(sum(catch_data$total_catch_wt_kg > 0), 1618)
  expect_equal(sum(catch_data$cpue_kg_km2), 438435.5, tolerance = 0.5)

  expect_equal(nrow(bio_data$length_data), 41983)
  expect_equal(sum(bio_data$length_data$Length_cm, na.rm = TRUE), 915385)
  expect_equal(nrow(bio_data$age_data), 178)
})

test_that("shortspine_afsc_slope", {
  catch_data <- pull_catch(
    common_name = "shortspine thornyhead",
    survey = "AFSC.Slope"
  )
  bio_data <- pull_bio(
    common_name = "shortspine thornyhead",
    survey = "AFSC.Slope"
  )

  expect_equal(nrow(catch_data), 1696)
  expect_equal(sum(catch_data$total_catch_wt_kg > 0), 1619)
  expect_equal(sum(catch_data$cpue_kg_km2), 1003391, tolerance = 0.1)

  expect_equal(nrow(bio_data$length_data), 75542)
  expect_equal(sum(bio_data$length_data$Length_cm, na.rm = TRUE), 1945800)
  expect_equal(nrow(bio_data$age_data), 16411)
})

test_that("shortspine_nwfsc_slope", {
  catch_data <- pull_catch(
    common_name = "shortspine thornyhead",
    survey = "NWFSC.Slope"
  )
  bio_data <- pull_bio(
    common_name = "shortspine thornyhead",
    survey = "NWFSC.Slope"
  )

  expect_equal(nrow(catch_data), 1714)
  expect_equal(sum(catch_data$total_catch_wt_kg > 0), 1536)
  expect_equal(round(sum(catch_data$cpue_kg_km2), 0), 899266)

  expect_equal(nrow(bio_data), 46990)
  expect_equal(sum(bio_data$Length_cm, na.rm = TRUE), 1215624, tolerance = 0.5)
})
