#' Plot proportion or count of responses
#'
#' @param df Table with data
#' @param x Name of the response level variable
#' @param y Name of variable with statistic, either proportion `P` or counts `N`
#' @param ymin Name of variable with lower limit for error bar, e.g., `LowerCI`. Optional.
#' @param ymax Name of variable with lower limit for error bar, e.g., `UpperCI`. Optional.
#' @param ribbon_ymin Name of variable with lower limit for ribbon, e.g., `PosteriorLowerCI`. Optional.
#' @param ribbon_ymax Name of variable with upper limit for ribbon, e.g., `PosteriorUpperCI`. Optional.
#' @param color Name of variable that maps color. Optional.
#' @param group Name of variable that maps grouping. Optional.
#' @param cutpoints Optional vector of estimated cut points. In case of the matrix, average as computed per column
#' @param fill_interval Fill colors for stripes that indicate intervals.
#'
#' @return ggplot
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_continuous
#' @importFrom rlang quo_is_null enquo as_string ensym
#' @importFrom grDevices col2rgb rgb
#'
#' @examples
#' data(aiq)
#' aiq_avg <- bootstrap_response_counts(aiq, Response ~ Group + Question, R = 100)
#' aiq_q1 <- dplyr::filter(aiq_avg, Question == Question[1])
#' plot_responses(aiq_q1, x = Response, y = P, color = Group)
#'
#' # with errorbars
#' plot_responses(aiq_q1, x = Response, y = P, ymin = LowerCI, ymax = UpperCI, color = Group)
#'
#' # with ribbon intead of error bars
#' plot_responses(aiq_q1, x = Response, y = P,
#'                ribbon_ymin = LowerCI, ribbon_ymax = UpperCI, color = Group)
#'
#' # with cutpoints from the model fit
#' data(aiq_draws)
#' cutpoints_draws <- extract_stan_posterior_matrix(aiq_draws, cutpoints)
#' plot_responses(aiq_q1, x = Response, y = P, ymin = LowerCI, ymax = UpperCI,
#'                color = Group, cutpoints = inv_logit(colMeans(cutpoints_draws)))
plot_responses <- function(df, x, y, ymin = NULL, ymax = NULL,  ribbon_ymin = NULL, ribbon_ymax = NULL, color = NULL, group = NULL, cutpoints = NULL, fill_interval = c("gray90", "gray85")) {
  response_nlevels <- nlevels(df[[rlang::as_string(rlang::ensym(x))]])
  if (!rlang::quo_is_null(rlang::enquo(color))) {
    color_nlevels <- nlevels(df[[rlang::as_string(rlang::ensym(color))]])
  } else {
    color_nlevels <- 1
  }

  # intervals based on cut points
  interval_df <- prepare_cutpoints(cutpoints, fill_interval, response_nlevels)
  df[['x']] <- interval_df$xmid[as.integer(df[[rlang::as_string(rlang::ensym(x))]])]

  # panel grid line color
  mid_values <- rowMeans(grDevices::col2rgb(fill_interval))
  mid_rgb <- grDevices::rgb(mid_values[1], mid_values[2], mid_values[3], maxColorValue = 255)

  # plot aesthetics
  prop_plot <-
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = {{ y }}, color = {{color}}, group = {{group}})) +
    geom_vstripe(data = interval_df, ggplot2::aes(x = x, xend = .data$xend), fill = interval_df$fill) +
    geom_grid_hlines(color = "white")

  # adding stripe, if the limits were provided
  if (!rlang::quo_is_null(rlang::enquo(ribbon_ymin)) & !rlang::quo_is_null(rlang::enquo(ribbon_ymax))) {
    prop_plot <- prop_plot + ggplot2::geom_ribbon(ggplot2::aes(ymin = {{ ribbon_ymin }}, ymax = {{ ribbon_ymax }}, fill = {{color}}), color = NA, alpha = 0.75 / color_nlevels)
  }

  # adding error bars, if the limits were provided
  if (!rlang::quo_is_null(rlang::enquo(ymin)) & !rlang::quo_is_null(rlang::enquo(ymax))) {
    prop_plot <- prop_plot + ggplot2::geom_errorbar(ggplot2::aes(ymin = {{ ymin }}, ymax = {{ ymax }}), width = 0.05)
  }

  # plotting lines
  prop_plot <- prop_plot + ggplot2::geom_line()


  # points
  prop_plot +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(name = rlang::as_string(rlang::ensym(x)), limits = c(0, 1), breaks = interval_df$xmid, labels = 1:response_nlevels, expand = c(0.02, 0.02))
}


# plot_responses(aiq_q1, x = Response, y = P, ymin = LowerCI, ymax = UpperCI, color = Group)
