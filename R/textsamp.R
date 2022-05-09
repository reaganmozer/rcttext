#' Select a random sample of documents
#'
#' Functions to select random samples of documents using different sampling schemes and/or
#' along different design criteria.
#'
#' @param x A \link{corpus} object or character vector of text documents.
#' @param size a non-negative integer giving the number of documents to sample.
#' @param prob a vector of probability weights for each document.
#' @param wt.fn a function for generating probability weights; ignored when \code{prob} is used. See Details.
#' @param method the following methods are implemented: simple random sampling without replacement (`srswor`),
#' simple random sampling with replacement (`srswr`), Poisson sampling (`poisson`), systematic sampling (`systematic`);
#' if \code{method} is missing, the default method is \code{srswor}.
#' @param scheme optional sampling scheme to implement
#' @return Returns a \code{data.frame} containing identifiers for the selected documents.
#' @export

textsamp <- function(x, size=length(x), prob=NULL, wt.fn=NULL, scheme=NULL,
                     method=c("srswr","srswor","systematic","poisson")){

}


#' @rdname textsamp
#' @param by a \code{data.frame} with document-level stratification variable(s) or character vector with names of variables in `docvars(x)`
#' @param ... additional arguments passed on to `textsamp`. Cannot include `scheme`.
#' @export
textsamp_strata <- function(x, by=NULL, ...){

}


#' @rdname textsamp
#' @param by a \code{data.frame} with document-level grouping variable(s) or character vector with names of variables in `docvars(x)`
#' @param ... additional arguments passed on to `textsamp`. Cannot include `scheme`.
#' @export
textsamp_cluster <- function(x, by=NULL, ...){

}
