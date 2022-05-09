#' Estimate treatment impacts for hybrid-scored text outcomes
#'
#' Given text from a randomized trial with a binary treatment, where a subset of
#' the documents have been human-scored, this function computes model-assisted
#' estimates for the average treatment effect with respect to the human-coded
#' outcome.
#'
#'
#' @param y.obs A vector of human-coded scores (with NAs for unscored documents).
#' @param yhat A vector of predicted scores estimated via \code{predict_scores}.
#' @param Z Indicator for treatment assignment.
#' @param wts Sampling weights for which documents were human scored.  Assumed uniform if null.
#' @param design Type of design used for random assignment (complete
#'   randomization, multisite randomized, cluster randomized, and blocked and cluster randomized).
#' @param siteID Vector of IDs for site, for multi-site randomized experiments.
#' @param clusterID Vector of IDs for cluster, for cluster-randomized experiments.
#' @param data A \code{data.frame} of subject-level identifiers, demographic
#'   variables, group membership, and/or other pre-treatment covariates.
#' @param adjust (optional) character vector or named list of variables in the data matrix to
#'   adjust for when estimating treatment impacts.
#' @return A model object for estimating treatment impact across an array of features.
#' @export

estimate_impacts <- function(y.obs, yhat, Z,
                             wts = NULL,
                             design=c("crd","multi","cluster","rcbd"),
                            siteID = NULL,
                            clusterID = NULL,
                            data, adjust=NULL){



}
