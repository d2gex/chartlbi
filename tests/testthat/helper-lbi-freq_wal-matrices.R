library("dplyr")

freq_df <- data.frame(
  meanlength = c(1, 2, 3, 4),
  X2021 = c(NA, 2, 3, 3),
  X2022 = c(2, 3, 3, 5),
  X2023 = c(2, 2, 2, 4)
)

wal_df <- data.frame(
  meanlength = c(1, 2, 3, 4),
  X2021 = c(4.3, 5.4, 6.6, 8),
  X2022 = c(4.4, 5.9, 6.6, 8),
  X2023 = c(4.3, 5.8, 7.1, 10)
)