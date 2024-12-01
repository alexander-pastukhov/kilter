#' Extract variable from draws table as a matrix
#'
#' @param draws_df Table in `draws_df` format
#' @param varname Variable name that corresponds to multiple columns in format `var[1]`, `var[2]`, etc. Can be written directly `varname` or as a string `"varname"`.
#'
#' @return Matrix
#' @export
#' @importFrom rlang as_string ensym
#' @importFrom dplyr select starts_with
#'
#' @examples
#' data(aiq_draws)
#' cutpoints <- extract_stan_posterior_matrix(aiq_draws, cutpoints)
#'
#' # same but using string
#' cutpoints <- extract_stan_posterior_matrix(aiq_draws, "cutpoints")
extract_stan_posterior_matrix <- function(draws_df, varname) {
  col_template <- paste0(rlang::as_string(rlang::ensym(varname)), "[")

  draws_df |>
    dplyr::select(dplyr::starts_with(col_template)) |>
    as.matrix()
}
