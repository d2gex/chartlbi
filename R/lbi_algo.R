library("R6")

LbiAlgo <- R6Class("LbiAlgo", public = list(
  data = NULL,
  params = NULL,

  #' Initialise the LBI class
  #'
  #' @param data length-frequency dataframe
  #' @param params R6 object with biological parameters and binwidth
  initialize = function (data, params) {
    self$data <- data
    self$params <- params
  },

  #' Run LBI algorithm.
  #'
  #' @returns A dataframe with all calculated indicators.
  run = function () {
    return (
      lb_ind_pretty(self$data,
                    self$params$binwidth,
                    self$params$linf,
                    self$params$l50)
    )
  }
))
