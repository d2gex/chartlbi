#'Class plotting LBI results
#'
#' @description
#' Results are plotted as a tower of plots, one on top of another

OutputPlotter <- R6::R6Class("OutputPlotter", public = list(

  data = NULL,
  plot_context = NULL,
  thresholds = NULL,
    # @formatter:off
  #'@description
  #' Initialise the OutputPlotter class
  #'
  #' @param data a dataframe with the LBI indicators
  #' @param plot_context A R6 object or named List with parameteres that allow customising certain aspects of the plot
  #' @param thresholds A name list with the thresholds against which the LBI indicators will be copmared
  #' @export
  # @formatter:on
  initialize = function(data, plot_context, thresholds) {
    self$data <- data
    self$plot_context <- plot_context
    self$thresholds <- thresholds
  },

  # @formatter:off
  #'@description
  #' Build a tower of plots
  #'
  #' @returns a ggplot class
  # @formatter:on
  build_plots = function() {
    return(private$build_lbi_tower())
  }
), private = list(

  # @formatter:off
  #' @description
  #'
  #' Build the title for a whole grid
  # @formatter:on
  build_grid_title = function(title, size = 15, just = "centre") {
    tgrob <- ggpubr::text_grob(title, size = size, just = just)
    return(ggpubr::ggarrange(plotlist = list(ggpubr::as_ggplot(tgrob)),
                             ncol = 1,
                             nrow = 1))
  },

  # @formatter:off
  #' @description
  #' Make Y axis of each single plot prettier
  #'
  #' @param name a string identifying the name of the y-axis to change
  #' @return pretty y-axis name
  # @formatter:on
  make_lbi_y_axis_prettier = function(name) {
    return(switch(name,
                  'Lc_Lmat' = bquote(L[c] / L[mat]),
                  'L25_Lmat' = bquote(L['25%'] / L[mat]),
                  'Lmax5_Linf' = bquote(L['max5%'] / L[inf]),
                  'Lmean_Lopt' = bquote(L[mean] / L[opt]),
                  'Lmean_Lfem' = bquote(L[mean] / L[FEM]),
                  'Pmega' = bquote(P[mega])))

  },

  #' @description
  #' Build a plot representing a single LBI indicator
  #'
  #' @details
  #' A plot is generated for each single indicator. The indicator a the bottom of the stack will provide
  #' the common ticks (years) and xlabel (Year label).
  #'
  #' @returns a ggplot class
  build_single_lbi_row_plot = function(data, colname, threshold, is_bottom) {
    g <- ggplot2::ggplot(data, ggplot2::aes(x = factor(years), y = .data[[colname]])) +
      ggplot2::geom_line(ggplot2::aes(group = 1)) +
      ggplot2::geom_point(ggplot2::aes(colour = fitness)) +
      ggplot2::geom_hline(yintercept = threshold,
                          linetype = 'dotted',
                          col = 'green',
                          linewidth = self$plot_context$threshold_line_width) +
      ggplot2::scale_color_manual(values = c(yes = "limegreen", no = 'red4')) +
      ggplot2::theme_bw() +
      ggplot2::xlab('Years') +
      ggplot2::ylab(private$make_lbi_y_axis_prettier(colname))

    if (is_bottom) {
      g <- g +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = self$plot_context$x_text_angle, size = self$plot_context$x_text_size),
          axis.text.y = ggplot2::element_text(angle = self$plot_context$y_text_angle, size = self$plot_context$y_text_size),
          axis.title.x = ggplot2::element_text(size = self$plot_context$x_title_size),
          legend.position = "none"
        )
    }
    else {
      g <- g +
        ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_text(angle = self$plot_context$y_text_angle, size = self$plot_context$y_text_size),
                       legend.position = "none")
    }
    g <- g +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = self$plot_context$y_title_size)) +
      ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
    return(g)
  },

  #' @description
  #' Build every plot for every indicator
  #'
  #' @returns A list of all built plots
  build_all_lbi_plots = function(data, thresholds) {
    col_names <- names(data)
    col_names <- col_names[col_names != 'years']
    plots <- list()
    for (i in seq_along(col_names)) {
      c_name <- col_names[[i]]
      col_threshold <- thresholds[[c_name]]
      col_data <- data.frame(years = data$years)
      col_data[[c_name]] <- data[[c_name]]
      if (stringr::str_detect(c_name, 'Lfem')) {
        col_data$fitness <- ifelse(col_data[[c_name]] >= col_threshold, 'yes', 'no')
      }
      else {
        col_data$fitness <- ifelse(col_data[[c_name]] > col_threshold, 'yes', 'no')
      }
      plots[[i]] <- private$build_single_lbi_row_plot(col_data,
                                                      c_name,
                                                      col_threshold,
                                                      is_bottom = ifelse(i == 1, TRUE, FALSE))
    }
    return(plots)
  },

  #' @description
  #' Mount a tower of plots (one on top of another)
  #'
  #' @returns A tower with all plots aligned
  build_lbi_tower = function() {
    plots <- private$build_all_lbi_plots(self$data, self$thresholds)
    tower <- ggpubr::ggarrange(plotlist = rev(plots),
                                ncol = 1,
                                nrow = length(self$thresholds))
    tower_title <- private$build_grid_title(title = self$plot_context$main_title,
                                            size = self$plot_context$main_title_size,
                                            just = self$plot_context$main_title_just)
    return(ggpubr::ggarrange(
      plotlist = list(tower_title, tower),
      ncol = 1,
      nrow = 2,
      heights = c(0.5, 10)
    ))
  }
))