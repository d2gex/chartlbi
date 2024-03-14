test_that("procedural LBI result == OOP LBI result", {
  params <- AlgoParams$new()
  params$linf <- 3
  params$l50 <- 2
  params$binwidth <- 1

  lbi_engine <- LbiAlgo$new(freq_df_sample, params)
  oop_result <- lbi_engine$lbi()
  procedural_result <- lb_ind_pretty(freq_df_sample, 1, 3, 2)
  expect_equal(oop_result, procedural_result)
})
