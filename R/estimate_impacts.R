#' Estimate treatment impacts for hybrid-scored text outcomes
#'
#' Given text from a randomized trial with a binary treatment, where a subset of
#' the documents have been human-scored, this function computes model-assisted
#' estimates for the average treatment effect with respect to the human-coded
#' outcome.
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
  # Input Checks: Ensure dimensions match, data is a dataframe, etc.

  # Create coded variable based on weights
  if(is.null(wts)) wts = rep(1/sum(!is.na(y.obs)),length(y.obs)) # uniform sampling if no weights provided
  coded = ifelse(is.na(y.obs), 0, wts)

  # Incorporate siteID and clusterID, if available
  dat = cbind(data, y.obs, yhat, Z, coded, pi.hat=wts)
  if(!is.null(siteID)) dat$siteID = siteID
  if(!is.null(clusterID)) dat$clusterID = clusterID


  # Get Model-Assisted and other estimates
  ests = get_ML_est(dat, coded=coded, yhat=yhat)

  # Adjust for covariates if provided
  if (!is.null(adjust)) {
    # Use your preferred regression method (e.g., lm, glm) to adjust the estimates.
    # Here's a basic example using lm:
    formula <- as.formula(paste0("ML.est ~ Z + ", paste(adjust, collapse = " + ")))
    fit <- lm(formula, data = dat)
    ests$ML.est.adj <- coef(fit)["Z"]
    # ... Add similar adjustments for other estimates if needed ...
  }


  # Prepare a comprehensive output (dataframe, list, etc.)
  # Include both unadjusted and adjusted estimates, standard errors, etc.

  return(ests)
}


