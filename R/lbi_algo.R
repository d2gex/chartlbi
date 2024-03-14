library("R6")

LbiAlgo <- R6Class("LbiAlgo", public = list(
  data = NULL,
  params = NULL,
  # @formatter:off
  #' Initialise the LBI class
  #'
  #' @param data length-frequency dataframe
  #' @param params R6 object with biological parameters and binwidth
  # @formatter:on
  initialize = function(data, params) {
    self$data <- data
    self$params <- params
  },
  lbi = function() {
    return(lb_ind_pretty(self$data,
                         self$params$binwidth,
                         self$params$linf,
                         self$params$l50))
  },

  # @formatter:off
  #' Run LBI algorithm.
  #'
  #' @returns A list with a vector of years and dataframe holding all calculated indicators
  # @formatter:on
  run = function() {
    results <- self$lbi()
    output <- data.frame(
      Lc_Lmat = results$Lc_Lmat,
      L25_Lmat = results$L25_Lmat,
      Lmax5_Linf = results$Lmax5_Linf,
      Pmega = results$Pmega,
      Lmean_Lopt = results$Lmean_Lopt,
      Lmean_Lfem = results$Lmean_LFeM
    )

    return(list(
      years = results$Year,
      estimates = output))

  }
))