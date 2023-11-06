
if (!is.null(var_col_sub)) {
  dat_bar_col_sub <- as.data.frame(table(var_col = var_col_sub),
                                   stringsAsFactors = FALSE) %>%
    mutate(var_col = factor(var_col, levels = levels(dat_bar_col$var_col)),
           x = as.numeric(var_col), y = max_freq - Freq / 2)
  # var_col_levels <- as.character(dat_bar_col$var_col)
  # var_col_sub_left <- setdiff(var_col_levels, unique(dat_bar_col_sub$var_col))
  # if (length(var_col_sub_left) > 0) {
  #   dat_bar_col_sub <- rbind(
  #     dat_bar_col_sub,
  #     data.frame(var_col = var_col_sub_left, Freq = 0)
  #   )
  # }
  # dat_bar_col_sub <- dat_bar_col_sub %>%
  #   mutate(x = as.numeric(var_col), y = max_freq - Freq / 2)
  grid.rect(x = unit(dat_bar_col_sub$x, "native"),
            y = unit(dat_bar_col_sub$Freq / 2, "native"),
            width = unit(0.99, "native"),
            height = unit(dat_bar_col_sub$Freq, "native"),
            gp = gpar(col = "black", fill = NA, lty = 2))
  if (position == "top") {
    grid.text(label = dat_bar_col_sub$Freq,
              x = unit(dat_bar_col_sub$x, "native"),
              y = unit(dat_bar_col_sub$Freq, "native") + unit(-0.1, "lines"),
              just = "bottom", gp = gpar(fontsize = 12, lineheight = 0.7))
  } else {
    grid.text(label = dat_bar_col_sub$Freq,
              x = unit(dat_bar_col_sub$x, "native"),
              y = unit(dat_bar_col_sub$Freq, "native") + unit(0.1, "lines"),
              just = "top", gp = gpar(fontsize = 12, lineheight = 0.7))
  }
}
