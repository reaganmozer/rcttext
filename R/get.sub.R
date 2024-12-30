
#' Calculate Subset Estimator
#'
#' This function calculates the subset estimator for a given dataset.
#' It estimates the treatment effect and its variance based on a
#' subset of the data.
#'
#' @param coded A binary vector indicating which samples are part of
#'   the subset (1) and which are not (0).
#' @param data A data frame containing the observed data. It must
#'   include the columns `Z` (treatment indicator), `Yobs` (observed
#'   outcome), and `pi.hat` (sampling probabilities).
#'
#' @return A data frame containing the estimated treatment effect
#'   (`est`), the standard error (`SE`), and the estimated standard
#'   error (`SEhat`).
#'
#' @import dplyr
#' @export
#'
#' @examples
#' # Example usage with hypothetical data
#' coded <- c(1, 0, 1, 0, 1)
#' data <- data.frame(Z = c(1, 0, 1, 0, 1),
#'                    Yobs = c(5, 3, 6, 2, 7),
#'                    pi.hat = c(0.8, 0.5, 0.9, 0.6, 0.7))
#' result <- get.sub(coded, data)
#' print(result)
#'
#' @export

get.sub <- function(coded, data) {

  # Calculate population-level variance of Yobs within each treatment group
  est.pop <- data %>%
    group_by(Z) %>%
    dplyr::summarise(
      n = n(),                  # Number of observations
      var = var(Yobs),          # Variance of Yobs
      .groups = "drop"
    )

  # Calculate the overall variance using the population estimates
  var <- sum(est.pop$var / est.pop$n)

  # Select the subset of data based on the coded vector
  coded.samp <- data[coded == 1,]

  # Calculate the treatment effect and variance within the subset
  est.part <- coded.samp %>%
    group_by(Z) %>%
    dplyr::summarise(
      Ybar = weighted.mean(Yobs, w = 1/pi.hat),  # Weighted mean of Yobs
      n = n(),                                   # Number of observations
      var = var(Yobs),                           # Variance of Yobs
      .groups = "drop"
    )

  # Estimate the treatment effect and standard errors
  est <- diff(est.part$Ybar)
  var.hat <- sum(est.part$var / est.part$n)

  # Return the estimated treatment effect and standard errors
  data.frame(est = est, SE = sqrt(var), SEhat = sqrt(var.hat))
}

#' Calculate Synthetic and Model-Assisted Estimators
#'
#' This function calculates synthetic and model-assisted estimators for a given dataset. It estimates the treatment effect and its variance using model-assisted techniques.
#'
#' @param yhat A vector of predicted outcomes from a model.
#'
