test_that("ICES and Pretty versions reports the same results", {

  # Generate 100 different length-frequency matrices to ensure the two functions are well tested
  freq_dfs <- multiple_freq_df_producer(freq_df_sample, 100)
  for (i in 1:length(freq_dfs)) {
    result_ices <- lb_ind_ices(data = freq_dfs[[i]],
                               binwidth = 1,
                               linf = 3,
                               lmat = 2,
                               weight = wal_df_sample)
    result_pretty <- lb_ind_pretty(freq_dfs[[i]],
                                   binwidth = 1,
                                   linf = 3,
                                   lmat = 2)
    trimmed_result_ices <- result_ices[, !colnames(result_ices) %in% c('Lmaxy_Lopt', 'Lmaxy')]
    expect_equal(trimmed_result_ices, result_pretty)

  }
})

