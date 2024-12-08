% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_stan_posterior_array.R
\name{parse_stan_posterior_array}
\alias{parse_stan_posterior_array}
\title{Parse Stan array parameter}
\usage{
parse_stan_posterior_array(draws, varname, columns, value_name = "Value")
}
\arguments{
\item{draws}{cmdstanr draws in "df" format}

\item{varname}{Name of the variable in draws}

\item{columns}{List of columns that correspond to array indexes.
Each columns is described via \verb{<name> = <vector of factor levels>}.
Each index is converted to a factor column with labelled factor levels.
If \verb{<vector of factor levels>} is \code{NULL}, column contains original integer indexes.}

\item{value_name}{Name of the value column, defaults to "Value"}
}
\value{
Table with columns \code{.chain}, \code{.draw}, \verb{<value_name>}, and columns described by \code{columns} parameter.
}
\description{
Parse Stan array parameter where each columns is in form \verb{<varname>[index1, index2, ...]>. Extracts the variable, pivots table into long format so that each index is a separate column. Name of each column and factor levels are defined via }columns` parameter.
}
\examples{
data(aiq)
data(aiq_draws)
group_alpha <- parse_stan_posterior_array(aiq_draws, a, list("Group" = levels(aiq$Group)))
}
