plot_time_series <- function(data, variable, color_by = "good", title = NULL) {
  royal_palette <- c("#4169E1", "#C1122F", "#6F8DFF", "#FF526B", "#D9B35F", "#7C9BFF")
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::ggplot(data, ggplot2::aes(
      x = .data$time,
      y = .data[[variable]],
      color = .data[[color_by]]
    )) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::scale_color_manual(values = rep(royal_palette, length.out = length(unique(data[[color_by]])))) +
      ggplot2::labs(x = "Time", y = variable, color = color_by, title = title) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#10182A", color = NA),
        panel.background = ggplot2::element_rect(fill = "#10182A", color = NA),
        panel.grid.major = ggplot2::element_line(color = "#263553"),
        panel.grid.minor = ggplot2::element_line(color = "#1A2742"),
        text = ggplot2::element_text(color = "#F4F7FF"),
        axis.text = ggplot2::element_text(color = "#9BA8C7"),
        legend.background = ggplot2::element_rect(fill = "#10182A", color = NA),
        legend.key = ggplot2::element_rect(fill = "#10182A", color = NA),
        legend.text = ggplot2::element_text(color = "#F4F7FF"),
        legend.title = ggplot2::element_text(color = "#9BA8C7"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = ggplot2::element_text(face = "bold", color = "#F4F7FF")
      )
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

plot_bar <- function(data, x, y, title = NULL, transparent = FALSE) {
  royal_palette <- c("#4169E1", "#C1122F", "#6F8DFF", "#FF526B", "#D9B35F", "#7C9BFF")
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plot_fill <- if (transparent) "transparent" else "#10182A"
    grid_color <- if (transparent) grDevices::adjustcolor("#9BA8C7", alpha.f = 0.18) else "#263553"
    ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[x]])) +
      ggplot2::geom_col(show.legend = FALSE, width = 0.46) +
      ggplot2::scale_fill_manual(values = rep(royal_palette, length.out = nrow(data))) +
      ggplot2::labs(x = NULL, y = y, title = title) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = plot_fill, color = NA),
        panel.background = ggplot2::element_rect(fill = plot_fill, color = NA),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = grid_color, linewidth = 0.25),
        panel.grid.minor = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        text = ggplot2::element_text(color = "#F4F7FF"),
        axis.text = ggplot2::element_text(color = "#9BA8C7"),
        plot.title = ggplot2::element_text(face = "bold", color = "#F4F7FF", size = 10),
        plot.margin = ggplot2::margin(4, 4, 4, 4)
      )
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
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(bg = "#10182A", mar = c(0, 0, 0, 0))
  graphics::plot.new()
  usr <- graphics::par("usr")
  graphics::rect(usr[1], usr[3], usr[2], usr[4], col = "#10182A", border = NA)
  graphics::text(0.5, 0.5, message, col = "#9BA8C7", cex = 1.1)
}
