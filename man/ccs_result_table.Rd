% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccs_utility_functions.R
\name{ccs_result_table}
\alias{ccs_result_table}
\title{Given a list of CCS model, produce a summary table of the results}
\usage{
ccs_result_table(model_list, corpus, Z)
}
\arguments{
\item{model_list}{A list of CCS models, or the result of a
ccs_list_table() call.}

\item{corpus}{The corpus (as a list of strings)}

\item{Z}{The binary treatment variable}
}
\value{
A data frame containing summary of the results.
}
\description{
Given a list of CCS model, produce a summary table of the results
}
