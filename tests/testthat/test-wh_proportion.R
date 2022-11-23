test_that("wh_proportion errors when 2 arguments are missing", {
  testthat::expect_error(wh_plot_proportion(dir = getwd()))
})

test_that("wh_proportion leads to correct files", {
  temporary_directory <- tempdir()
  output_catch <- wh_plot_proportion(
    data_catch = catch_nwfsc_combo,
    dir = temporary_directory
  )
  testthat::expect_length(output_catch, 2)
  testthat::expect_true(all(file.exists(output_catch)))
  testthat::expect_true(all(grepl("presence-absence", output_catch)))

  output_bio <- wh_plot_proportion(
    data_bio = bio_nwfsc_combo,
    dir = temporary_directory
  )
  testthat::expect_length(output_bio , 2)
  testthat::expect_true(all(file.exists(output_bio)))
  testthat::expect_true(all(grepl("sex", output_bio)))
  unlink(output_bio, force = TRUE)

  output_all <- wh_plot_proportion(
    data_catch = catch_nwfsc_combo,
    data_bio = bio_nwfsc_combo,
    dir = temporary_directory
  )
  testthat::expect_length(output_all , 4)
  testthat::expect_true(all(file.exists(output_all)))
  testthat::expect_true(all(grepl("presence|sex", output_all)))
  unlink(output_all, force = TRUE)
})
