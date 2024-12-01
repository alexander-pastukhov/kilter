#' Compute summary based on both boostrapped samples and of posterior predictive distribution
#'
#' Computes bootstrapped summary via [bootstrap_response_counts()] and posterior predictions via
#' [posterior_predictions_for_response_counts()], then joins them on fixed factors and response variable
#' as specified by the formula
#'
#' @param df Table with data
#' @param formula Formula that specifies outcome variable, fixed and random effects.
#' @param eta Matrix of samples_n x nrow(df), each row is a single posterior sample
#' @param cutpoints Matrix of samples_n x nlevels(df$outcome) - 1, each row is a single posterior sample
#' @param ci_variable Which variable to use for percentile confidence intervals, either `"N"` or `"P"` (default).
#' @param RBoot Number of samples for bootstrapping, default is `2000`.
#' @param RPost Number of posterior samples to process, `NULL` (default) means all samples are used.
#' @param CI Percentile confidence interval, default is `0.97`
#' @param .progress Logical, whether to show progress bar during bootstrapping. Default is `TRUE`.
#'
#' @return data.frame or tibble with fixed effects columns, PosteriorMean, PosteriorMedian, PosteriorLowerCI, and PosteriorUpperCI.
#' @export
#' @importFrom rlang .data
#' @importFrom purrr map list_rbind
#' @importFrom dplyr group_by_at summarize
#' @importFrom stats median
#'
#' @examples
#' data(aiq)
#' data(aiq_draws)
#' eta_draws <- extract_stan_posterior_matrix(aiq_draws, eta)
#' cutpoints_draws <- extract_stan_posterior_matrix(aiq_draws, cutpoints)
#' aiq_avg <-
#'   compute_bootstrap_and_posterior_predictions(aiq,
#'                                               Response ~ Group + Question,
#'                                               eta_draws,
#'                                               cutpoints_draws,
#'                                               RBoot=10,
#'                                               RPost=10)
#' @seealso [bootstrap_response_counts()] [posterior_predictions_for_response_counts()]
compute_bootstrap_and_posterior_predictions <- function(df, formula, eta, cutpoints, ci_variable = "P", RBoot = 2000, RPost = NULL, CI = 0.97, .progress = TRUE) {
  # bootstrap part
  boot_avg <- bootstrap_response_counts(df, formula, ci_variable, R = RBoot, CI = CI, .progress = .progress)

  # posterior predictions part
  post_avg <- posterior_predictions_for_response_counts(df, formula, eta, cutpoints, ci_variable, R = RPost, CI = CI, .progress = .progress)

  # joing them on fixed terms + response
  formula_terms <- parse_formula(formula)
  dplyr::full_join(boot_avg, post_avg, by = c(formula_terms$fixed, formula_terms$outcome))
}
