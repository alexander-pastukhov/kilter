#' Compute total count and proportion responses for each level per combination of factors
#'
#' Compute total count and proportion of responses for each level per combination of factors
#' defined as a formula: `<response_var> ~ <fixed_factor1> +  <fixed_factor2> + ... (1|<random_factor1>) + (1|<random_factor2>) + ...`.
#' The `<response_var>` must be a factor, as the function fills in missing response level counts with zeros.
#'
#' The data is first grouped by all fixed and random factors and responses are counted per factor level. Any missing combinations are
#' filled in with zero counts. Next, function computes proportion of each response level for all fixed and random factors combinations.
#' Finally, it groups data only by fixed factors and compute total counts and averagere proportion of responses per factor level.
#'
#' Optionally, the data can be resampled with replacement before counting. To resample over entire table set `resample = TRUE`. Alternatively,
#' you can resample within data group by using a formula: `resample = ~ <var1> + <var2> + ...`. Here, the data is first grouped and then resampled.
#'
#' @param df Table with data
#' @param formula Formula that specifies outcome variable, fixed and random effects.
#' @param resample logical or formula. `resample = TRUE` : resampled with replacement over entire table. `resample = ~ <var1> + <var2> + ...` resample over grouped data.
#' @param predictions vector or NULL. Optional values that _replace_ original response values. Useful for applying the same counting procedure for model predictions.
#'
#' @return tibble with columns for all fixed factors and response variable, as well as columns `N` (total count) and `P` (proportion of responses for this level).
#' @export
#' @importFrom dplyr group_by_at count mutate ungroup summarize slice_sample
#' @importFrom rlang syms is_formula .data
#' @importFrom tidyr complete
#'
#' @examples
#' data(art)
#'
#' counts <- count_responses(art, Response ~ Group + PaintingKind + Scale)
#'
#' # with resampling over entire table
#' count_sample <- count_responses(art, Response ~ Group + PaintingKind + Scale, resample = TRUE)
#'
#' # with resampling per group
#' count_sample <- count_responses(art, Response ~ Group + PaintingKind + Scale, resample = ~ Group)
count_responses <- function(df, formula, resample = FALSE, predictions = NULL) {
  # figure out terms
  formula_terms <- parse_formula(formula)

  # check that variables actually exist
  check_that_columns_exist(df, formula_terms$all)

  # check that outcome variable is a factor
  if (!is.factor(df[[formula_terms$outcome]])) stop(sprintf("Response column %s must be a factor.", formula_terms$outcome))

  # resample, if required
  if (is.logical(resample)) {
    if (resample) df <- dplyr::slice_sample(df, prop = 1, replace = TRUE)
  } else if (rlang::is_formula(resample)) {
    # parse formula
    reasample_terms <- parse_formula(resample)

    # check that formula makes sense
    if (!is.null(reasample_terms$outcome)) stop("resample formula should not contain an outcome variable")
    if (!is.null(reasample_terms$random)) stop("resample formula should not contain random factors")
    check_that_columns_exist(df, reasample_terms$fixed)

    # resample within each group
    df <-
      df |>
      dplyr::group_by_at(reasample_terms$fixed) |>
      dplyr::slice_sample(prop = 1, replace = TRUE) |>
      dplyr::ungroup()
  } else stop("Incomprehensible value for resample parameter.")

  # use predictions and first check that it is not combined with resampling.
  if (!is.null(predictions)) {
    if (!(is.logical(resample) & !resample)) stop("You can either resample and provide predictions, not both.")

    df[[formula_terms$outcome]] <- factor(predictions, levels = levels(df[[formula_terms$outcome]]))
  }


  df |>
    # count responses
    dplyr::group_by_at(c(formula_terms$fixed, formula_terms$random, formula_terms$outcome)) |>
    dplyr::count() |>

    # fill in missing response level counts (they are zero)
    dplyr::group_by_at(c(formula_terms$fixed, formula_terms$random)) |>
    tidyr::complete(!!!rlang::syms(c(formula_terms$outcome)), fill = list("n" = 0)) |>

    # compute proportion of responses
    dplyr::mutate(P = .data$n / sum(.data$n)) |>
    dplyr::ungroup() |>

    # compute only for fixed factors averaging over random ones
    dplyr::group_by_at(c(formula_terms$fixed, formula_terms$outcome)) |>
    dplyr::summarize(N = sum(.data$n),
                     P = mean(.data$P),
                     .groups = "drop")
}
