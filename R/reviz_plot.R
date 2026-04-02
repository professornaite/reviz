#' Visualize relationships with model overlay
#'
#' @param data A data.frame
#' @param x X variable name
#' @param y Y variable name
#' @param z Optional grouping variable for color/facets
#' @param model "lm" or "loess"
#' @param use_color TRUE for color, FALSE for facets
#'
#' @return ggplot object
#' @export
reviz_plot <- function(data, x, y, z = NULL, model = c("lm", "loess"), use_color = TRUE) {
  model <- match.arg(model)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_point(alpha = 0.7, size = 2) +
    ggplot2::geom_smooth(method = model, se = TRUE, linewidth = 1.2) +
    ggplot2::theme_minimal()

  if (!is.null(z) && nzchar(z)) {
    if (use_color) {
      p <- p + ggplot2::aes(color = .data[[z]])
    } else {
      p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", z)))
    }
  }

  p + ggplot2::labs(title = paste(y, "~", x))
}
