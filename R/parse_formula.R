#' Parse formula into outcome, fixed, and random effects
#'
#' Parse formula in format outcome ~ fixed_effect1 + fixed_effect2 + ... (1 | random_effect1) + (1 | random_effect2) + ...
#'
#' @param formula Formula that describes variables
#'
#' @return list with `outcome`, `fixed`, `random` and `all` fields
#' @export
#' @importFrom stats terms
#'
#' @examples
#' # only one fixed effect
#' parse_formula(y ~ x)
#'
#' # two fixed effects
#' parse_formula(y ~ x1 + x2)
#'
#' # one random effect
#' parse_formula(y ~ (1|z))
#'
#' # one fixed and two random effects
#' parse_formula(y ~ x + (1|z1) + (1|z2))
parse_formula <- function(formula) {
  # Get the terms object
  terms_obj <- terms(formula)

  # Extract all terms
  all_terms <- attr(terms_obj, "term.labels")

  # Separate fixed and random effects
  fixed_effects <- all_terms[!grepl("\\|", all_terms)] # Fixed effects: no '|'

  # Process random effects
  # Split terms containing '|'
  random_effects_raw <- all_terms[grepl("\\|", all_terms)]
  random_effects <- unlist(strsplit(random_effects_raw, "\\+")) # Split combined random effects
  random_effects <- trimws(sub(".*\\|", "", random_effects))    # Extract variable names after '|'
  if (length(random_effects) == 0) random_effects <- NULL

  # outcome
  outcome <- attr(terms_obj, "response")  # Get the index of the response variable
  if (outcome == 0) {
    outcome_var <- NULL
  } else {
    outcome_var <- as.character(attr(terms_obj, "variables")[[outcome + 1]])  # Extract its name
  }


  list(
    outcome = outcome_var,
    fixed = fixed_effects,
    random = random_effects,
    all = c(fixed_effects, random_effects, outcome_var)
  )
}
