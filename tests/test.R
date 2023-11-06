dat <- data.frame(a = sample(x = letters, size = 100, replace = TRUE),
                  b = sample(x = 1:10, size = 100, replace = TRUE))

dev.off()
row_levels <- plot_evid_map(var_row = dat$a, var_col = dat$b,
                            margins = c(1, 4, 4, 1))

dev.off()
plot_evid_map(var_row = dat$a, var_col = dat$b, var_row_levels = letters,
              margins = c(1, 4, 4, 1))

dev.off()
plot_bar_row(var_row = dat$a, var_row_levels = row_levels,
             margins = c(1, 4, 4, 1))

dev.off()
plot_bar_col(var_col = dat$b, position = "bottom")
