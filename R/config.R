#' Plot's context details class
#'
#' @description
#' A class that holds all the aspects that shape the final plot visualising the
#' results from the LBI algorithm.
PlotContext <- R6::R6Class("PlotContext", public = list(
  main_title_size = NULL,
  main_title = NULL,
  threshold_line_width = NULL,
  x_title_size = NULL,
  x_text_size = NULL,
  x_text_angle = NULL,
  y_title_size = NULL,
  y_text_size = NULL,
  y_text_angle = NULL,

  # @formatter:off
  #' @description
  #' Initialise the PlotContext class. Default values for the input parameters are enough to graph the
  #' results for the first time.
  #'
  #' @export
  # @formatter:on
  initialize = function(main_title_size = 15,
                        main_title = "LBI Scores",
                        threshold_line_width = 1.2,
                        x_title_size = 18,
                        x_text_size = 12,
                        x_text_angle = 90,
                        y_title_size = 18,
                        y_text_size = 12,
                        y_text_angle = 0) {
    self$main_title_size <- main_title_size
    self$main_title <- main_title
    self$threshold_line_width <- threshold_line_width
    self$x_title_size <- x_title_size
    self$x_text_size <- x_text_size
    self$x_text_angle <- x_text_angle
    self$y_title_size <- y_title_size
    self$y_text_size <- y_text_size
    self$y_text_angle <- y_text_angle
  }
))

#' Thresholds list
#'
#' @description
#' A list that holds all thresholds over which the LBI indicators are measured against.
#' @export
Thresholds <- list(
  Lc_Lmat = 1,
  L25_Lmat = 1,
  Lmax5_Linf = 0.8,
  Pmega = 0.3,
  Lmean_Lopt = 0.9,
  Lmean_Lfem = 1
)

