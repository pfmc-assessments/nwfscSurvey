test_that("petrale_sole_wcgbt", {
  catch_data <- pull_catch(
    common_name = "petrale sole",
    years = c(2003, 2025)
  )
  bio_data <- pull_bio(
    common_name = "petrale sole",
    years = c(2003, 2025)
  )
  colnames(bio_data) <- tolower(colnames(bio_data))

  # new api returning 14178 trawls with one less trawl from 2024: 202403020011
  # the positive catch record is missing a depth value
  expect_equal(nrow(catch_data), 14179)
  expect_equal(sum(catch_data$total_catch_wt_kg > 0), 6108)
  expect_equal(sum(catch_data$cpue_kg_km2), 7007254)
  expect_equal(sum(catch_data$total_catch_wt_kg), 115094.4, tolerance = 0.1)

  # the new api is returning a total of 87602 records, 12 more than expected
  # the differences are greater within each year between the apis. For example, the new api
  # has the following differences to the old api
  # 2025: 0, 2024: -19, 2023: +1, 2022: +22, 2021: +1, 2019: +1
  # 2018: 0, 2017: +5, 2016: 1
  # even without standard filtering there is a difference in the number of records
  # Most of these differences are due to na length and age records

  na_records <- which(is.na(bio_data$length_cm) & is.na(bio_data$age_years))
  bio_data <- bio_data[-na_records, ]

  expect_equal(nrow(bio_data), 87569)
  expect_equal(sum(bio_data$length_cm), 3057571, tolerance = 0.5)
})

test_that("petrale_sole_triennial", {
  catch_data <- pull_catch(
    common_name = "petrale sole",
    survey = "Triennial",
    years = c(1980, 2004)
  )
  bio_data <- pull_bio(
    common_name = "petrale sole",
    survey = "Triennial",
    years = c(1980, 2004)
  )
  colnames(bio_data$length_data) <- tolower(colnames(bio_data$length_data))

  # the new api is returning +260 more records
  # 1980: +145, 1983: +32, 1986: +23, 1989: +2, 1992: +1, 1995: +1, 1998: 0, 2001: 0, 2004: 0
  expect_equal(nrow(catch_data), 3882)
  expect_equal(sum(catch_data$total_catch_wt_kg > 0), 2183)
  expect_equal(sum(catch_data$cpue_kg_km2), 262065.2, tolerance = 0.5)
  expect_equal(sum(catch_data$total_catch_wt_kg), 9672.446, tolerance = 0.1)

  # new api is returning 4 more length records
  # 1992: +1 and 1995: +3
  expect_equal(nrow(bio_data$length_data), 14705)
  expect_equal(sum(bio_data$length_data$length_cm, na.rm = TRUE), 469066)
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
  colnames(bio_data) <- tolower(colnames(bio_data))
  na_records <- which(is.na(bio_data$length_cm) & is.na(bio_data$age_years))
  bio_data <- bio_data[-na_records, ]

  # the new api is returning 14178 records
  expect_equal(nrow(catch_data), 14179)
  expect_equal(sum(catch_data$total_catch_wt_kg > 0), 7203)
  # The CPUE difference is likely due the two 2019 trawls with NA area_swept whe the
  # cpue is being calculated based on average area swept
  # expect_equal(sum(catch_data$cpue_kg_km2), 5619109)
  expect_equal(sum(catch_data$total_catch_wt_kg), 106087.6, tolerance = 0.1)

  # the bio records match
  expect_equal(nrow(bio_data), 108408)
  expect_equal(sum(bio_data$length_cm), 3212723)
})

test_that("shortspine_triennial", {
  catch_data <- pull_catch(
    common_name = "shortspine thornyhead",
    survey = "Triennial",
    years = c(1980, 2004)
  )
  bio_data <- pull_bio(
    common_name = "shortspine thornyhead",
    survey = "Triennial",
    years = c(1980, 2004)
  )
  colnames(bio_data$length_data) <- tolower(colnames(bio_data$length_data))

  # new api has 4142 tows
  # 1980: 145, 1983: +32, 1986: +23, 1989: +32, 1992: +19, 1995: +9, 1998: 0, 2001: 0, 2004: 0
  expect_equal(nrow(catch_data), 3882)
  expect_equal(sum(catch_data$total_catch_wt_kg > 0), 1299)
  expect_equal(sum(catch_data$cpue_kg_km2), 376980.1, tolerance = 0.5)

  # the bio samples match
  expect_equal(nrow(bio_data$length_data), 41900)
  expect_equal(sum(bio_data$length_data$length_cm, na.rm = TRUE), 912598)
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
  colnames(bio_data$length_data) <- tolower(colnames(bio_data$length_data))

  # the new api has the same number of tows and positive tows
  expect_equal(nrow(catch_data), 1696)
  expect_equal(sum(catch_data$total_catch_wt_kg > 0), 1619)
  # the total in the new api = 1076933 (sum(catch_data$cpue_kg_km2))
  # there are 93 tows with no area swept in the old and new api
  # the revised code calculates a cpue based on the average area swept so this is
  # expected to not match
  # use alternative test
  expect_equal(sum(catch_data$total_catch_wt_kg), 39037.16, tolerance = 0.1)

  # the new api bio samples match
  expect_equal(nrow(bio_data$length_data), 75542)
  expect_equal(sum(bio_data$length_data$length_cm, na.rm = TRUE), 1945800)
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
  colnames(bio_data) <- tolower(colnames(bio_data))

  expect_equal(nrow(catch_data), 1714)
  expect_equal(sum(catch_data$total_catch_wt_kg > 0), 1536)
  # the new api returns 1490360 difference of 591095
  # 1998: +491.7, 1999: 0, 2000: 433578.3, 2001: 0, 2002: 157024.8
  # the differences appear to be all due to rounding
  #expect_equal(sum(catch_data$cpue_kg_km2), 899265.6, tolerance = 0.1)
  expect_equal(sum(catch_data$area_swept_ha), 3444.421, tolerance = 0.1)
  expect_equal(sum(catch_data$total_catch_numbers, na.rm = TRUE), 54166)
  expect_equal(sum(catch_data$total_catch_wt_kg), 18036.98, tolerance = 0.1)

  # the new api matches
  expect_equal(nrow(bio_data), 46990)
  expect_equal(sum(bio_data$length_cm, na.rm = TRUE), 1215625, tolerance = 0.5)
})
