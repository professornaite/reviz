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

  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = model, se = TRUE, size = 1.2) +
    theme_minimal()

  if (!is.null(z) && z != "") {
    if (use_color) {
      p <- p + aes_string(color = z)
    } else {
      p <- p + facet_wrap(as.formula(paste("~", z)))
    }
  }

  p + labs(title = paste(y, "~", x))
}
