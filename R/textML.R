#' Model-assisted impact analysis through hybrid human/machine text scoring
#'
#' A wrapper function for the multiple steps of generating features, training a
#' scoring model on the human-coded data, predicting scores, and comparing human
#' v. machine estimates.
#'
#' This function takes in a corpus of text documents (or a set of computed text
#' features) along with a sample of human-coded outcome values, and trains an
#' ensemble of machine learning models to predict the outcome as a function of
#' the machine measures of text.
#'
#' @inheritParams estimate_impacts
#' @param x a corpus or character vector of text documents.
#' @param y a vector of human-coded scores. Set elements to `NA` for documents
#'   not previously scored.
#' @param z optional indicator for treatment assignment. If specified, separate
#'   ensembles will be trained for each treatment group;
#' @param max.features maximum number of text features to use for model
#'   training. Defaults to `NULL` (no strict limit)
#' @param ... additional arguments passed to \link{train}.
#' @return a \code{textML} model object
#' @export

textML <- function(x, y, z=NULL,
                   wts = NULL,
                   design=c("crd","multi","cluster","rcbd"),
                   siteID = NULL,
                   clusterID = NULL,

                   max.features=NULL, ...){


}
