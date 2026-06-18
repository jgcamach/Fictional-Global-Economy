plot_time_series <- function(data, variable, color_by = "good", title = NULL) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::ggplot(data, ggplot2::aes(
      x = .data$time,
      y = .data[[variable]],
      color = .data[[color_by]]
    )) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::labs(x = "Time", y = variable, color = color_by, title = title) +
      ggplot2::theme_minimal(base_size = 13)
  } else {
    groups <- unique(data[[color_by]])
    colors <- grDevices::hcl.colors(length(groups), "Dark 3")
    y_range <- range(data[[variable]], na.rm = TRUE)
    graphics::plot(
      NA,
      xlim = range(data$time, na.rm = TRUE),
      ylim = y_range,
      xlab = "Time",
      ylab = variable,
      main = title
    )
    for (i in seq_along(groups)) {
      group_data <- data[data[[color_by]] == groups[i], ]
      graphics::lines(group_data$time, group_data[[variable]], col = colors[i], lwd = 2)
    }
    graphics::legend("topright", legend = groups, col = colors, lwd = 2, bty = "n")
  }
}

plot_bar <- function(data, x, y, title = NULL) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[x]])) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::labs(x = NULL, y = y, title = title) +
      ggplot2::theme_minimal(base_size = 13)
  } else {
    old_mar <- graphics::par("mar")
    on.exit(graphics::par(mar = old_mar), add = TRUE)
    graphics::par(mar = c(7, 4, 3, 1))
    graphics::barplot(
      height = data[[y]],
      names.arg = data[[x]],
      las = 2,
      col = grDevices::hcl.colors(nrow(data), "Set 2"),
      ylab = y,
      main = title
    )
  }
}

format_ore_total <- function(data, good) {
  value <- sum(data$output[data$good == good])
  format(round(value, 1), big.mark = ",")
}

plot_empty_graph <- function(message = "Add a series to begin") {
  graphics::plot.new()
  graphics::text(0.5, 0.5, message)
}
