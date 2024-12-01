#' Compute mean and non-parametric bootstrapped percentile confidence intervals
#'
#' @param df Table with data
#' @param formula Formula that specifies outcome variable, fixed and random effects.
#' @param ci_variable Which variable to use for percentile confidence intervals, either `"N"` or `"P"` (default).
#' @param R Number of samples for bootstrapping, default is `2000`.
#' @param CI Percentile confidence interval, default is `0.97`
#' @param .progress Logical, whether to show progress bar during bootstrapping. Default is `TRUE`.
#'
#' @return data.frame or tibble with fixed effects columns, as well as N, P, LowerCI, and UpperCI.
#' @export
#' @importFrom purrr map list_rbind
#' @importFrom dplyr group_by_at mutate left_join
#' @importFrom rlang .data
#'
#' @examples
#' avg_prop <- bootstrap_response_counts(art, Response ~ Group + PaintingKind + Scale, R = 10)
#'
#' avg_count <- bootstrap_response_counts(art,
#'                                        Response ~ Group + PaintingKind + Scale,
#'                                        ci_variable = "N",
#'                                        R = 10)
#' @seealso [count_responses()], [lower_ci()], [upper_ci()]
bootstrap_response_counts <- function(df, formula, ci_variable = "P", R = 2000, CI = 0.97, .progress = TRUE) {
  # check that ci_variable is valid
  if (!(ci_variable %in% c("N", "P"))) stop('Invalid CI variable, must be "N" or "P"')

  # bootstrap samples
  samples <- purrr::map(1:R, ~count_responses(df, formula, resample = TRUE), .progress = .progress) |> purrr::list_rbind()

  # parse formula to figure out grouping (we don't need to check it here, as an incorrect one would have generated error in count_responses)
  formula_terms <- parse_formula(formula)

  samples |>
    # compute confidence intervals from bootstrap samples
    dplyr::group_by_at(c(formula_terms$fixed, formula_terms$outcome)) |>
    dplyr::summarize(LowerCI = lower_ci(.data[[ci_variable]], CI = CI),
                     UpperCI = upper_ci(.data[[ci_variable]], CI = CI),
                     .groups = "drop") |>

    # add true means
    dplyr::left_join(count_responses(df, formula), by = c(formula_terms$fixed, formula_terms$outcome))
}
