##' @name ordered_logistic
##' @rdname ordered_logistic
##'
##' @title Ordered logistic distribution
##'
##' @param k response level
##' @param eta vector of means
##' @param c vector of cutpoints
##' @param n number of observations
##' @param log logical, if `TRUE` probabilities p are given as `log(p)`
##'
##' @return `dordered_logistic` gives the density, `rordered_logistic` gives random value.
NULL

##' @name ordered_logistic
##' @export
##' @examples
##' dordered_logistic(2, 0.5, c(-2, -1, 0, 1, 2))
dordered_logistic <- function(k, eta, c, log = FALSE) {
  padded_cutpoints <- c(-Inf, c, Inf)

  upper_cut <- inv_logit(padded_cutpoints[k + 1] - eta)
  lower_cut <- inv_logit(padded_cutpoints[k] - eta)
  p <- upper_cut - lower_cut

  if (log) return(log(p))
  p
}


##' @name ordered_logistic
##' @export
##' @importFrom purrr map_int
##' @examples
##' rordered_logistic(1, 0.5, c(-2, -1, 0, 1, 2))
rordered_logistic <- function(n, eta, cutpoints) {
  k <- 1:(length(cutpoints) + 1)

  if (length(eta) == 1) {
    p <- dordered_logistic(k, eta, cutpoints)
    y <- sample(k, size = n, replace = TRUE, prob = p)
  } else {
    if (n > length(eta)) eta <- rep(eta, ceiling(n / length(eta)))[1:n]
    y <- purrr::map_int(eta, ~sample(k, size = 1, prob = dordered_logistic(k, ., cutpoints)))
  }
  y
}
