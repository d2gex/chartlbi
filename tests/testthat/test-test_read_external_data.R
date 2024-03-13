test_that("reading_external_data_works", {
  data <- test_path("wal_gad_duplicate_biomass_at_age.rds")
  expect_equal(nrow(data) , 102)
})
