library("R6")
library("ggplot2")
library("ggpubr")
library("stringr")

OutputPlotter <- R6Class("OutputPlotter", public = list(

  data = NULL,
  thresholds = NULL,
  initialize = function(data, thresholds) {
    self$data <- data
    self$thresholds <- thresholds
  },
  generate_outputs = function(title_size, title) {
    return(private$build_lbi_tower(title_size, title))
  }
), private = list(

  build_grid_title = function(title, size = 15, just = "centre") {
    tgrob <- text_grob(title, size = size, just = just)
    return(ggarrange(plotlist = list(as_ggplot(tgrob)),
                     ncol = 1,
                     nrow = 1))
  },

  # @formatter:off
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
  build_single_lbi_row_plot = function(data, colname, threshold, is_bottom) {
    g <- ggplot(data, aes(x = factor(years), y = .data[[colname]])) +
      geom_line(aes(group = 1)) +
      geom_point(aes(colour = fitness)) +
      geom_hline(yintercept = threshold, linetype = 'dotted', col = 'green', linewidth = 1.2) +
      scale_color_manual(values = c(yes = "limegreen", no = 'red4')) +
      theme_bw() +
      xlab('Years') +
      ylab(private$make_lbi_y_axis_prettier(colname))

    if (is_bottom) {
      g <- g +
        theme(
          axis.text.x = element_text(angle = 90, size = 15),
          axis.title.x = element_text(size = 18),
          legend.position = "none"
        )
    }
    else {
      g <- g +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              legend.position = "none")
    }
    g <- g + theme(axis.title.y = element_text(size = 18))
    return(g)
  },
  build_all_lbi_plots = function(data, thresholds) {
    col_names <- names(data)
    col_names <- col_names[col_names != 'years']
    plots <- list()
    for (i in seq_along(col_names)) {
      c_name <- col_names[[i]]
      col_threshold <- thresholds[[c_name]]
      col_data <- data.frame(years = data$years)
      col_data[[c_name]] <- data[[c_name]]
      if (str_detect(c_name, 'Lfem')) {
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
  build_lbi_tower = function(title_size, title) {
    plots <- private$build_all_lbi_plots(self$data, self$thresholds)
    tower <- ggarrange(plotlist = rev(plots),
                       ncol = 1,
                       nrow = length(self$thresholds))
    tower_title <- private$build_grid_title(title, size = title_size)
    return(ggarrange(
      plotlist = list(tower_title, tower),
      ncol = 1,
      nrow = 2,
      heights = c(0.5, 10)
    ))
  }
))