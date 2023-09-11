light_grey <- function() {
  "grey90"
}
#' @import grid
#' @import dplyr
#' @import magrittr
#' @export
plot_evid_map <- function(var_row, var_col, var_row_levels = NULL,
                          print_var_row_levels = TRUE, position_var_col = "top",
                          n_y = NULL,
                          color_range = c("white", "steelblue"),
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
  fill_map <- colorRampPalette(color_range)(max(dat_map$Freq) + 1)
  dat_map$fill <- fill_map[dat_map$Freq + 1]
  pushViewport(plotViewport(margins = margins,
                            xscale = c(0.5, n_x + 0.5),
                            yscale = c(0.5, n_y + 0.5)))
  grid.rect(x = unit(dat_map$x_num, "native"), y = unit(dat_map$y_num, "native"),
            width = unit(1, "native"), height = unit(1, "native"),
            gp = gpar(col = "white", fill = dat_map$fill))
  grid.text(label = dat_map$Freq,
            x = unit(dat_map$x_num, "native"), y = unit(dat_map$y_num, "native"),
            gp = gpar(fontsize = 12,
                      col = ifelse(dat_map$Freq == 0, light_grey(), "black")))
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
#' @import grid
#' @import dplyr
#' @import magrittr
#' @export
plot_bar_row <- function(var_row, var_row_levels = NULL,
                         margins = c(0.5, 0, 4, 4)) {
  # dat_bar_row <- as.data.frame(table(var_row = var_row))
  # if (!is.null(var_row_levels)) {
  #
  # }
  # n_y <- length(unique(var_row))
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
            gp = gpar(col = "white", fill = "steelblue"))
  grid.text(label = dat_bar_row$Freq,
            x = unit(x_max - dat_bar_row$Freq, "native") + unit(-0.1, "lines"),
            y = unit(dat_bar_row$y_num, "native"),
            just = "right",
            gp = gpar(fontsize = 12, lineheight = 0.7,
                      col = ifelse(dat_bar_row$Freq == 0, light_grey(), "black")))
  popViewport()
}
#' @import grid
#' @import dplyr
#' @import magrittr
#' @export
plot_bar_col <- function(var_col, margins = c(0.5, 0, 4, 4)) {
  dat_bar_col <- as.data.frame(table(var_col = var_col)) %>%
    mutate(x = as.numeric(var_col), y = max(Freq) - Freq / 2)
  n_x <- length(unique(var_col))
  pushViewport(plotViewport(margins = margins,
                            yscale = c(0, max(dat_bar_col$Freq)),
                            xscale = c(0.5, n_x + 0.5)))
  grid.rect(x = unit(dat_bar_col$x, "native"),
            y = unit(dat_bar_col$Freq / 2, "native"),
            width = unit(0.99, "native"),
            height = unit(dat_bar_col$Freq, "native"),
            gp = gpar(col = "white", fill = "steelblue"))
  grid.text(label = dat_bar_col$Freq,
            x = unit(dat_bar_col$x, "native"),
            y = unit(dat_bar_col$Freq, "native") + unit(0.1, "lines"),
            just = "bottom", gp = gpar(fontsize = 12, lineheight = 0.7))
  popViewport()
}
