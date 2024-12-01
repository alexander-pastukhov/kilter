#' Compute lower limit for percentile confidence interval
#'
#' @param x values
#' @param CI confidence interval, defaults to 0.97
#'
#' @return float
#' @export
#' @importFrom stats quantile
#'
#' @examples
#' # default 97% CI
#' lower_ci(rnorm(100))
#'
#' # alternative CI
#' lower_ci(rnorm(100), CI=0.89)
lower_ci <- function(x, CI=0.97) {
  quantile(x, (1 - CI) / 2)
}

#' Compute upper limit for percentile confidence interval
#'
#' @param x values
#' @param CI confidence interval, defaults to 0.97
#'
#' @return float
#' @export
#' @importFrom stats quantile
#'
#' @examples
#' # default 97% CI
#' upper_ci(rnorm(100))
#'
#' # alternative CI
#' upper_ci(rnorm(100), CI=0.89)
upper_ci <- function(x, CI=0.97) {
  quantile(x, 1 - (1 - CI) / 2)
}
