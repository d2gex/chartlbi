test_that("matrix contains two max values", {
  freq_x_wal_df <- freq_df * wal_df
  max_value <- max(freq_x_wal_df, na.rm = TRUE)
  num_max_values <- 2
  expect_equal(
    sum((freq_x_wal_df == max_value), na.rm = TRUE), num_max_values
  )
})
