#' Plot estimated intensity as box plots
#'
#' @param df Table with estimated response intensity
#' @param intensity Variable for estimated response intensity
#' @param cutpoints Vector with estimated cut points
#' @param group Variable for grouping for data or `NULL` if there is only a single estimate
#' @param fill_interval Fill colors for stripes that indicate intervals.
#' @param x_labels_pos Either `"bottom"` (default) or `"top"`. The latter is useful for combined plots.
#'
#' @export
#' @importFrom ggplot2 ggplot geom_boxplot aes dup_axis coord_flip scale_y_continuous scale_x_continuous
#' @importFrom rlang as_string ensym
#'
#' @examples
#' data("aiq")
#' data("aiq_draws")
#'
#' # extract estimated intensity
#' intensity_df <- parse_stan_posterior_array(aiq_draws, a, list("Group" = levels(aiq$Group)))
#' intensity_df[["Intensity"]] <- inv_logit(intensity_df[["Value"]])
#'
#' # extract cutpoints
#' cutpoints <- inv_logit(colMeans(extract_stan_posterior_matrix(aiq_draws, cutpoints)))
#' plot_intensity(intensity_df, Intensity, cutpoints, group = Group)
#'
#' # ignoring grouping
#' plot_intensity(intensity_df, Intensity, cutpoints)
plot_intensity <- function(df, intensity, cutpoints, group = NULL, fill_interval = c("gray90", "gray85"), x_labels_pos = "bottom"){
  interval_df <- prepare_cutpoints(cutpoints, fill_interval)

  if (!rlang::quo_is_null(rlang::enquo(group))) {
    df[['x']] <- as.integer(df[[rlang::as_string(rlang::ensym(group))]])
    group_nlevels <- nlevels(df[[rlang::as_string(rlang::ensym(group))]])
    group_labels <- levels(df[[rlang::as_string(rlang::ensym(group))]])
  } else {
    df[['x']] <- 1
    group_nlevels <- 1
    group_labels <- "Intensity"
  }


  the_plot <-
    ggplot2::ggplot(df, aes(x = .data$x, y = {{intensity}}, group = {{group}}, color = {{group}}, fill = {{group}})) +
      geom_vstripe_flip(data = interval_df, ggplot2::aes(y = .data$x, yend = .data$xend), fill = interval_df$fill) +
      ggplot2::geom_boxplot(alpha = 0.75, show.legend = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_continuous(name = NULL, breaks = 1:group_nlevels, labels = group_labels)

      ggplot2::scale_y_continuous(name = NULL, limits = c(0, 1), expand = c(0.02, 0.02), breaks = interval_df$xmid, labels = 1:nrow(interval_df))

  if (x_labels_pos == "bottom") {
      the_plot <- the_plot + scale_y_continuous(name = NULL, limits = c(0, 1), expand = c(0.02, 0.02), breaks = interval_df$xmid, labels = 1:nrow(interval_df))
  } else if (x_labels_pos == "top") {
      the_plot <- the_plot + scale_y_continuous(name = NULL, limits = c(0, 1), expand = c(0.02, 0.02), breaks = interval_df$xmid, labels = NULL, sec.axis = ggplot2::dup_axis(labels = 1:nrow(interval_df)))
  } else stop(sprintf("Unrecognized value %s for x_labels_pos parameter", x_labels_pos))

  the_plot
}
