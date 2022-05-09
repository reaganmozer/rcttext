#' Given text from a randomized trial with a binary treatment, this function computes
#' estimates for the average treatment effect with respect to an array of text-based outcomes
#'
#'
#' @param x A character vector of text documents or a feature matrix returned by \link{tada}
#' @param Z Indicator for treatment assignment.
#' @param adj (optional) character vector or named list of variables in the data matrix to
#'   adjust for when estimating treatment impacts.
#' @param data A \code{data.frame} of subject-level identifiers, demographic
#'   variables, group membership, and/or other pre-treatment covariates.
#' @param design For multi-site and cluster randomized experiments, a named list of vectors containing site IDs and/or cluster IDs.
#' @param wts Sampling weights for documents.  Assumed uniform if null.
#' @param mcp character string specifying the correction method to be applied to adjust for multiple comparisons. Defaults to no adjustments. See \link{p.adjust} for available adjustment methods.
#' @return A model object for estimating treatment impact across an array of features.

#' @export

textfx <- function(x, Z,adj=NULL,data,
                   mcp = "none", wts = NULL,
                   design=list(siteID=NULL, clusterID=NULL)){



}
