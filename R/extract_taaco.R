#' Manage and merge text features generated using TAACO
#'
#' Tools to support feature extraction using TAACO.
#' \code{extract_taaco()} reads output and log files produced by TAACO program and returns a \code{data.frame} that can be merged with other feature sets.
#'
#' @param file Filename where TAACO results are stored
#' @param data Optional \code{data.frame} with additional document-level variables to include in output.
#' @param idvar If \code{data} is specified, character vector with name(s) of variables used for merging.
#' @return Returns a \code{data.frame} of text features.
#' @export
extract_taaco <- function(file, data=NULL, idvar=NULL){


}

