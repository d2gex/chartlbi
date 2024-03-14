library("R6")

freq_df_sample <- data.frame(
  meanlength = c(1, 2, 3, 4),
  X2021 = c(NA, 2, 3, 3),
  X2022 = c(2, 3, 3, 2),
  X2023 = c(2, 2, 4, 2),
  X2024 = c(4, 1, NA, 3)
)

wal_df_sample <- data.frame(
  meanlength = c(1, 2, 3, 4),
  X2021 = c(4.3, 5.4, 6.6, 8),
  X2022 = c(4.4, 5.9, 6.6, 8),
  X2023 = c(4.3, 5.8, 7, 14),
  X2024 = c(4.5, 5.1, 6.9, 12)
)

multiple_freq_df_producer <- function (freq_df, num_times) {

  freq_df_list <- list()
  for (i in 1:num_times) {
    df <- freq_df
    col_or_row <- sample(1:2, 1)
    values <- sample(1:100, 4)
    if (col_or_row == 1) {
      position <- sample(2:length(df), 1)
      df[,position] <- values
    }
    else {
      position <- sample(1:nrow(df), 1)
      df[position, -1] <- values
    }
    freq_df_list[[i]] <- df
  }
  return (freq_df_list)
}

AlgoParams <- R6Class("AlgoParams", public = list(
  binwidth = NULL,
  linf = NULL,
  l50 = NULL
))
