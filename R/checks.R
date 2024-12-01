#' Check that all columns are present in a table
#'
#' Check that all columns are present in a table, raises exception if any columns are missing
#'
#' @param df data.frame or tibble
#' @param column_names vector with column names
#'
#' @return NULL
#' @export
#'
#' @examples
#' data(art)
#' check_that_columns_exist(art, c("Group", "ID"))
check_that_columns_exist <- function(df, column_names) {
  missing_columns <- setdiff(column_names, names(df))

  if(length(missing_columns) > 0) stop(paste("Following variables are missing in the table:", paste(missing_columns, collapse = ", ")))
}


#' Check validity of variable for CI computation
#'
#' Check that variable for CI computation is either "P" or "N". Stop otherwise.
#'
#' @param ci_variable character
#'
#' @return NULL
#' @export
#'
#' @examples
#' check_ci_variable("P")
check_ci_variable <- function(ci_variable) {
  if (!(ci_variable %in% c("N", "P"))) stop('Invalid CI variable, must be "N" or "P"')
}
