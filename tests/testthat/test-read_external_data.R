test_that("reading external rds works", {
  data <- readRDS(test_path("wal_gad_duplicate_biomass_at_age.rds"))
  expect_equal(nrow(data), 101)
})
