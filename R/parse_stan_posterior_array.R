#' Parse Stan array parameter
#'
#' Parse Stan array parameter where each columns is in form `<varname>[index1, index2, ...]>.
#' Extracts the variable, pivots table into long format so that each index is a separate column.
#' Name of each column and factor levels are defined via `columns` parameter.
#'
#' @param draws cmdstanr draws in "df" format
#' @param varname Name of the variable in draws
#' @param columns List of columns that correspond to array indexes.
#' Each columns is described via `<name> = <vector of factor levels>`.
#' Each index is converted to a factor column with labelled factor levels.
#' If `<vector of factor levels>` is `NULL`, column contains original integer indexes.
#' @param value_name Name of the value column, defaults to "Value"
#'
#' @return Table with columns `.chain`, `.draw`, `<value_name>`, and columns described by `columns` parameter.
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr as_tibble select mutate starts_with
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_int
#' @importFrom stringr str_extract_all
#'
#' @examples
#' data(aiq)
#' data(aiq_draws)
#' group_alpha <- parse_stan_posterior_array(aiq_draws, a, list("Group" = levels(aiq$Group)))
parse_stan_posterior_array <- function(draws, varname, columns, value_name = "Value") {
  varname_template <- paste0(rlang::as_string(rlang::ensym(varname)))

  # Turn wide-array into a long table and extract indexes for each (ex)column
  long_table <-
    draws |>
    dplyr::as_tibble() |>
    dplyr::select(c(".chain", ".draw", dplyr::starts_with(varname_template))) |>
    tidyr::pivot_longer(dplyr::starts_with(varname_template), names_to = "Term", values_to = value_name) |>
    dplyr::mutate(Indexes = stringr::str_extract_all(.data$Term, "\\d+"))

  # check that we have consistent columns info
  if (length(long_table[["Indexes"]][[1]]) != length(columns)){
    stop(sprintf("Number of columns (%d) does not match number of indexes (%d)", (length(columns)), length(long_table[["Indexes"]][[1]])))
  }

  # turning each index into a factor column
  for(icol in 1:length(columns)) {
    if (!is.null(columns[[icol]])) {
      long_table[[names(columns)[icol]]] <- factor(purrr::map_int(long_table[["Indexes"]], ~as.integer(.[[icol]])), labels = columns[[icol]])
    } else {
      long_table[[names(columns)[icol]]] <- purrr::map_int(long_table[["Indexes"]], ~as.integer(.[[icol]]))
    }
  }

  # drop service columns
  long_table |>
    dplyr::select(-c("Term", "Indexes"))
}

