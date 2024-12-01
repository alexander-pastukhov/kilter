
#' Inverse logit
#'
#' Inverse logit, wrapper for `1 / (1 + exp(-x))`
#'
#' @param x Numeric vector
#'
#' @return Numeric vector
#' @export
#'
#' @examples
#' inv_logit(-Inf)
#' inv_logit(0)
#' inv_logit(Inf)
inv_logit <- function (x)
{
  1 / (1 + exp(-x))
}
