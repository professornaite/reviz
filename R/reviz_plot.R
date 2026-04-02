#' Visualize relationships with model overlay
#'
#' @param data A data.frame.
#' @param x X variable name.
#' @param y Y variable name.
#' @param z Optional grouping variable for color or facets.
#' @param model Smoother type: "lm" or "loess".
#' @param use_color TRUE for color grouping, FALSE for faceting.
#'
#' @return A ggplot object.
#' @export
reviz_plot <- function(data, x, y, z = NULL, model = c("lm", "loess"), use_color = TRUE) {
  model <- match.arg(model)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  if (!x %in% names(data)) {
    stop("`x` is not a column in `data`.", call. = FALSE)
  }

  if (!y %in% names(data)) {
    stop("`y` is not a column in `data`.", call. = FALSE)
  }

  if (!is.null(z) && nzchar(z) && !z %in% names(data)) {
    stop("`z` is not a column in `data`.", call. = FALSE)
  }

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[x]], y = .data[[y]])
  )

  if (!is.null(z) && nzchar(z)) {
    if (use_color) {
      p <- p +
        ggplot2::geom_point(
          ggplot2::aes(color = .data[[z]]),
          alpha = 0.7,
          size = 2
        ) +
        ggplot2::geom_smooth(
          ggplot2::aes(color = .data[[z]]),
          method = model,
          se = TRUE,
          linewidth = 1.2
        ) +
        ggplot2::labs(color = z)
    } else {
      p <- p +
        ggplot2::geom_point(alpha = 0.7, size = 2, color = "#2E86AB") +
        ggplot2::geom_smooth(
          method = model,
          se = TRUE,
          linewidth = 1.2,
          color = "#A23B72"
        ) +
        ggplot2::facet_wrap(stats::as.formula(paste("~", z)))
    }
  } else {
    p <- p +
      ggplot2::geom_point(alpha = 0.7, size = 2, color = "#2E86AB") +
      ggplot2::geom_smooth(
        method = model,
        se = TRUE,
        linewidth = 1.2,
        color = "#A23B72"
      )
  }

  p +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste(y, "~", x),
      x = x,
      y = y
    )
}
