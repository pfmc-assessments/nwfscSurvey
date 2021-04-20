test_that("Data frame structure of PullSpp.fn() is consistent", {
  allspp <- PullSpp.fn()
  expect_is(allspp, "data.frame")
  expect_equal(
    colnames(allspp),
    c("latin", "common", "common_name", "scientific_name")
  )
  expect_length(
    grep("unident$", allspp[["common"]]),
    0
  )
  expect_vector(
    object = grep("unident", allspp[["common"]]),
    ptype = integer(),
    size = 188
  )
})

test_that("Information for sablefish is consistent", {
  spp <- PullSpp.fn()
  sablefish <- spp[grep("sablefish", spp[["common"]]), ]
  expect_length(sablefish, 4)
  expect_equal(NROW(sablefish), 2)
  expect_equal(sablefish[["latin"]][1], "Anoplopoma fimbria")
})
