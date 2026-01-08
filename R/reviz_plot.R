#' Visualize relationships with model overlay
#'
#' @param data A data.frame.
#' @param x Name of x variable (string).
#' @param y Name of y variable (string).
#'
#' @return A ggplot object.
#' @export
reviz_plot <- function(data, x, y) {

  p <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes_string(x = x, y = y)
  ) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::geom_smooth(method = "lm", se = TRUE)

  p
}
