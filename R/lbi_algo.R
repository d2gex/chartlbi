#'LBI Algorithm Class
#'
#' @description
#' class wrapping and calling the LBI algorithm

LbiAlgo <- R6::R6Class("LbiAlgo", public = list(
  data = NULL,
  params = NULL,
  # @formatter:off
  #'@description
  #' Initialise the LBI class
  #'
  #' @param data length-frequency dataframe
  #' @param params R6 object or named List with biological parameters and binwidth
  #' @export
  # @formatter:on
  initialize = function(data, params) {
    self$data <- data
    self$params <- params
  },

  #' @description
  #'
  #' Wrap up and call the actual LBI algorithm function
  #'
  #' @details
  #' This method can be called in isolation to get the exact dataframe that the original LBI function returns.
  #' However, this method has been modified so that the weight-at-length matrix is never used and therefore
  #' Lmaxy and Lmaxy/Lopt ratio is no longer available.
  #'
  #' @returns A dataframe with all calculated indicators.
  lbi = function() {
    return(lb_ind_pretty(self$data,
                         self$params$binwidth,
                         self$params$linf,
                         self$params$l50))
  },

  # @formatter:off
  #'@description
  #' Run LBI algorithm.
  #'
  #' @returns A modified dataframe holding all calculated indicators.
  # @formatter:on
  run = function() {
    results <- self$lbi()
    estimates <- data.frame(
      years = results$Year,
      Lc_Lmat = results$Lc_Lmat,
      L25_Lmat = results$L25_Lmat,
      Lmax5_Linf = results$Lmax5_Linf,
      Pmega = results$Pmega,
      Lmean_Lopt = results$Lmean_Lopt,
      Lmean_Lfem = results$Lmean_LFeM
    )
    return(estimates)
  }
))