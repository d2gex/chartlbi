test_that("ICES and Pretty versions reports the same results", {

  # Generate 100 different length-frequency matrices to ensure the two functions are well tested
  for (i in 1:100) {
    col_or_row <- sample(1:2, 1)
    values <- sample(1:100, 4)
    if (col_or_row == 1) {
      position <- sample(2:length(freq_df), 1)
      freq_df[,position] <- values
    }
    else {
      position <- sample(1:nrow(freq_df), 1)
      freq_df[position, -1] <- values
    }
    result_ices <- lb_ind_ices(data = freq_df,
                               binwidth = 1,
                               linf = 3,
                               lmat = 2,
                               weight = wal_df)
    result_pretty <- lb_ind_pretty(freq_df,
                                   binwidth = 1,
                                   linf = 3,
                                   lmat = 2)
    expect_equal(result_ices[, !colnames(result_ices) %in% c('Lmaxy_Lopt', 'Lmaxy')], result_pretty)
  }
})

