#' Prepare data frame with interval limits and fill
#'
#' @param cutpoints Vector with cutpoints (one fewer than responses)
#' @param fill_interval Fill colors for intervals
#' @param response_nlevels Number of levels (intervals). Optional, only used with cut points are not supplied.
#'
#' @return Table with `response_nlevels` rows and three columns `x`, `xend`, and `fill`
#' @noRd
#'
#' @examples
#' prepare_cutpoints(5, c(0.1, 0.4, 0.7, 0.9), c("gray90", "gray85"))
prepare_cutpoints <- function(cutpoints, fill_interval, response_nlevels = NULL) {
  if (is.null(cutpoints)) {
    interval_x <- seq(0, 1, length.out = response_nlevels + 1)
  } else {
    interval_x <- c(0, unname(cutpoints), 1)
    response_nlevels <- length(cutpoints) + 1
  }
  data.frame(x = interval_x[1:response_nlevels],
             xmid = (interval_x[1:response_nlevels] + interval_x[2:(response_nlevels + 1)]) / 2,
             xend = interval_x[2:(response_nlevels + 1)],
             fill = rep(fill_interval, ceiling(response_nlevels / length(fill_interval)))[1:response_nlevels])

}

