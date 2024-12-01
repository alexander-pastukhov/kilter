#' Compute summary of posterior predictive distribution for the data given posterior distributions of eta and cutpoints
#'
#' @param df Table with data
#' @param formula Formula that specifies outcome variable, fixed and random effects.
#' @param eta Matrix of samples_n x nrow(df), each row is a single posterior sample
#' @param cutpoints Matrix of samples_n x nlevels(df$outcome) - 1, each row is a single posterior sample
#' @param ci_variable Which variable to use for percentile confidence intervals, either `"N"` or `"P"` (default).
#' @param R Number of samples to process, `NULL` (default) means all samples are used.
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
#' aiq_posterior_avg <-
#'   posterior_predictions_for_response_counts(aiq,
#'                                             Response ~ Group + Question,
#'                                             eta_draws,
#'                                             cutpoints_draws,
#'                                             R=10)
posterior_predictions_for_response_counts <- function(df, formula, eta, cutpoints, ci_variable = "P", R = NULL, CI = 0.97, .progress = TRUE) {
  # compute samples counts and averages (includes validity check for formula)
  sample_avg <- count_responses(df, formula)

  # check that ci_variable is valid
  check_ci_variable(ci_variable)

  # check that number of rows for eta and cupoints matches
  if (nrow(eta) != nrow(cutpoints)) stop("Different number of rows for eta and cutpoints parameters")

  # check that number of cutpoints is one less than number of levels in outcome variable
  formula_terms <- parse_formula(formula)
  if (ncol(cutpoints) != (nlevels(df[[formula_terms$outcome]]) - 1)) stop(sprintf("There should be one cutpoint fewer than levels in outcome variable. It is %d for %s variable (%d) and %d for cutpoints", nlevels(df[[formula_terms$outcome]]), formula_terms$outcome,  ncol(cutpoints)))

  # how many samples we want to process, all by default but fewer can be used for debugging
  if (is.null(R)) R <- nrow(eta)

  n <- ncol(eta)
  samples <-
    purrr::map(1:R, ~count_responses(df, formula, predictions = rordered_logistic(n, as.numeric(eta[., ]), as.numeric(cutpoints[., ]))), .progress = .progress) |>
    purrr::list_rbind()

  samples |>
    # compute confidence intervals from bootstrap samples
    dplyr::group_by_at(c(formula_terms$fixed, formula_terms$outcome)) |>
    dplyr::summarize(PosteriorMean = mean(.data[[ci_variable]]),
                     PosteriorMedian = stats::median(.data[[ci_variable]]),
                     PosteriorLowerCI = lower_ci(.data[[ci_variable]], CI = CI),
                     PosteriorUpperCI = upper_ci(.data[[ci_variable]], CI = CI),
                     .groups = "drop")
}
