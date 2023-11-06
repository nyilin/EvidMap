#' Define 'light grey' for 0 in evidence gap maps
#' @return Hard coded to be grey90.
light_grey <- function() {
  "grey90"
}
#' Plot an evidence gap map for frequency based on two variables
#' @param var_row The variable to be plotted as rows.
#' @param var_col The variable to be plotted as columns.
#' @param var_row_levels Ordering of levels in \code{var_row}. Can include
#'   values not in \code{var_row_levels} (in which case 0 will be plotted). The
#'   first level will be plotted in the top row. Default is \code{NULL}, i.e.,
#'   all unique values in \code{var_row} in alphabetic order.
#' @param print_var_row_levels Whether to print levels in \code{var_row} on the
#'   left of evidence gap map (default is \code{TRUE}). Useful when plotting
#'   multiple maps side by side with the same row labels.
#' @param position_var_col Where to print levels of \code{var_col} (default is
#'   \code{"top"}, can also choose \code{"bottom"}).
#' @param n_y Height of the evidence gap map. Default is \code{NULL}, i.e.,
#'   based on the number of levels in \code{var_row}. Can also choose larger
#'   integer values. Useful when plotting evidence maps of different sizes side
#'   by side.
#' @param color_range A vector of color for 0 and maximum frequency in the
#'   evidence gap map. Default is \code{c("white", "steelblue")}.
#' @param max_freq Maximum frequency corresponding to \code{color_range[2]}.
#'   Default is \code{NULL}, i.e., based on data. Can also choose a larger
#'   integer value. Useful when plotting multiple evidence maps and want to
#'   ensure the same color brightness refers to the same frequency.
#' @param margins Margins for the evidence gap map, in the order of bottom,
#'   left, top, right (see \code{\link[grid]{plotViewport}}).
#' @return A plot made using the \code{grid} package.
#' @import grid
#' @import dplyr
#' @import magrittr
#' @export
plot_evid_map <- function(var_row, var_col, var_row_levels = NULL,
                          print_var_row_levels = TRUE, position_var_col = "top",
                          n_y = NULL,
                          color_range = c("white", "steelblue"), max_freq = NULL,
                          color_zero = "grey90", fill_zero = NULL,
                          margins = c(0.5, 4, 4, 0.5)) {
  position_var_col <- match.arg(arg = tolower(position_var_col),
                                choices = c("top", "bottom"))
  if (is.null(var_row_levels)) {
    dat_mat_x <- as.data.frame(table(var_row = var_row)) %>%
      arrange(-Freq, as.character(var_row))
    var_row_levels <- as.character(dat_mat_x$var_row)
    dat_map <- as.data.frame(table(x = var_col, y = var_row)) %>%
      mutate(y = factor(y, levels = var_row_levels))
    n_x <- length(unique(dat_map$x))
  } else {
    # Given var_row_levels. If any level is not in data, include in plot with value 0
    dat_map <- as.data.frame(table(x = var_col, y = var_row))
    n_x <- length(unique(dat_map$x))
    var_row_levels_left <- setdiff(var_row_levels, unique(dat_map$y))
    if (length(var_row_levels_left) > 0) {
      dat_map <- rbind(
        dat_map,
        data.frame(x = rep(unique(dat_map$x), length(var_row_levels_left)),
                   y = rep(var_row_levels_left, each = n_x),
                   Freq = 0)
      )
    }
    dat_map$y <- factor(dat_map$y, levels = var_row_levels)
  }
  if (is.null(n_y)) n_y <- length(unique(dat_map$y))
  dat_map$x_num <- as.numeric(dat_map$x)
  dat_map$y_num <- n_y - as.numeric(dat_map$y) + 1
  if (is.null(max_freq)) max_freq <- max(dat_map$Freq)
  fill_map <- colorRampPalette(color_range)(max_freq + 1)
  dat_map$fill <- fill_map[dat_map$Freq + 1]
  if (!is.null(fill_zero)) {
    dat_map$fill[which(dat_map$Freq == 0)] <- fill_zero
  }
  pushViewport(plotViewport(margins = margins,
                            xscale = c(0.5, n_x + 0.5),
                            yscale = c(0.5, n_y + 0.5)))
  grid.rect(x = unit(dat_map$x_num, "native"), y = unit(dat_map$y_num, "native"),
            width = unit(1, "native"), height = unit(1, "native"),
            gp = gpar(col = "white", fill = dat_map$fill))
  grid.text(label = dat_map$Freq,
            x = unit(dat_map$x_num, "native"), y = unit(dat_map$y_num, "native"),
            gp = gpar(fontsize = 12,
                      col = ifelse(dat_map$Freq == 0, color_zero, "black")))
  if (position_var_col == "top") {
    grid.text(label = levels(dat_map$x), gp = gpar(fontsize = 12, lineheight = 0.7),
              x = unit(1:n_x, "native"), y = unit(1, "npc") + unit(0.5, "lines"),
              just = "left", rot = 90)
  } else {
    grid.text(label = levels(dat_map$x), gp = gpar(fontsize = 12, lineheight = 0.7),
              x = unit(1:n_x, "native"), y = unit(0, "npc") + unit(-0.5, "lines"),
              just = "right", rot = 90)
  }
  if (print_var_row_levels) {
    grid.text(label = levels(dat_map$y), gp = gpar(fontsize = 12, lineheight = 0.7),
              y = unit(n_y:1, "native"), x = unit(0, "npc") + unit(-0.5, "lines"),
              just = "right")
  }
  popViewport()
  return(var_row_levels)
}
#' Plot bar plot on the left of an evidence gap map for row frequency.
#' @inheritParams plot_evid_map
#' @param fill Filling color for the bar plot. Default is \code{"steelblue"}.
#' @import grid
#' @import dplyr
#' @import magrittr
#' @export
plot_bar_row <- function(var_row, var_row_levels = NULL, fill = "steelblue",
                         color_zero = "grey90",
                         margins = c(0.5, 0, 4, 4)) {
  if (!is.factor(var_row)) var_row <- as.factor(var_row)
  n_y <- nlevels(var_row)
  dat_bar_row <- as.data.frame(table(var_row = var_row)) %>%
    arrange(-Freq, as.character(var_row)) %>%
    mutate(var_row = factor(var_row, levels = var_row),
           y = as.numeric(var_row), x = max(Freq) - Freq / 2,
           y_num = n_y - y + 1)
  x_max <- max(dat_bar_row$Freq)
  pushViewport(plotViewport(margins = margins,
                            xscale = c(0, max(dat_bar_row$Freq)),
                            yscale = c(0.5, n_y + 0.5)))
  grid.rect(x = unit(dat_bar_row$x, "native"),
            y = unit(dat_bar_row$y_num, "native"),
            width = unit(dat_bar_row$Freq, "native"),
            height = unit(0.99, "native"),
            gp = gpar(col = "white", fill = fill))
  grid.text(label = dat_bar_row$Freq,
            x = unit(x_max - dat_bar_row$Freq, "native") + unit(-0.1, "lines"),
            y = unit(dat_bar_row$y_num, "native"),
            just = "right",
            gp = gpar(fontsize = 12, lineheight = 0.7,
                      col = ifelse(dat_bar_row$Freq == 0, color_zero, "black")))
  popViewport()
}
#' Plot bar plot on the top/bottom of an evidence gap map for column frequency.
#' @inheritParams plot_evid_map
#' @inheritParams plot_bar_row
#' @import grid
#' @import dplyr
#' @import magrittr
#' @export
plot_bar_col <- function(var_col, position = "top", max_freq = NULL,
                         fill = "steelblue", margins = c(0.5, 0, 4, 4)) {
  position <- match.arg(arg = tolower(position), choices = c("top", "bottom"))
  dat_bar_col <- as.data.frame(table(var_col = var_col)) %>%
    mutate(x = as.numeric(var_col))
  n_x <- length(unique(var_col))
  if (is.null(max_freq)) max_freq <- max(dat_bar_col$Freq)
  dat_bar_col$y <- max_freq - dat_bar_col$Freq / 2
  if (position == "top") {
    pushViewport(plotViewport(margins = margins,
                              yscale = c(0, max_freq),
                              xscale = c(0.5, n_x + 0.5)))
  } else {
    pushViewport(plotViewport(margins = margins,
                              yscale = c(max_freq,0),
                              xscale = c(0.5, n_x + 0.5)))
  }
  grid.rect(x = unit(dat_bar_col$x, "native"),
            y = unit(dat_bar_col$Freq / 2, "native"),
            width = unit(0.99, "native"),
            height = unit(dat_bar_col$Freq, "native"),
            gp = gpar(col = "white", fill = fill))
  if (position == "top") {
    grid.text(label = dat_bar_col$Freq,
              x = unit(dat_bar_col$x, "native"),
              y = unit(dat_bar_col$Freq, "native") + unit(0.1, "lines"),
              just = "bottom", gp = gpar(fontsize = 12, lineheight = 0.7))
  } else {
    grid.text(label = dat_bar_col$Freq,
              x = unit(dat_bar_col$x, "native"),
              y = unit(dat_bar_col$Freq, "native") + unit(-0.1, "lines"),
              just = "top", gp = gpar(fontsize = 12, lineheight = 0.7))
  }
  popViewport()
}
