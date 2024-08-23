#' Calculate Synthetic and Model-Assisted Estimators
#'
#' This function calculates synthetic and model-assisted estimators, along with their variances, based on the provided data.
#'
#' @param yhat A numeric vector of predicted outcomes for the entire dataset.
#' @param coded A binary vector indicating which documents have been hand-coded (1 for coded, 0 for not coded).
#' @param data A data frame containing the full dataset, including observed outcomes (`Yobs`), predicted outcomes (`yhat`),
#'        treatment indicator (`Z`), and propensity scores (`pi.hat`).
#'
#' @return A data frame with the following components:
#' \itemize{
#'   \item \code{est}: The effect estimate using the model-assisted estimator.
#'   \item \code{SE}: The standard error of the true variance of the estimate.
#'   \item \code{SEhat}: The estimated standard error of the estimate based on partial coding.
#' }
#'
#' @import dplyr
#' @export
#'
#' @examples
#' # Example usage with hypothetical data
#' data <- data.frame(Yobs = rnorm(100), Z = sample(c(0, 1), 100, replace = TRUE), pi.hat = runif(100))
#' yhat <- rnorm(100)
#' coded <- sample(c(0, 1), 100, replace = TRUE)
#' results <- get.ests(yhat, coded, data)
#' print(results)
#' @export

get.ests <- function(yhat, coded, data) {

  # Prepare the data by adding predicted outcomes (yhat), coding indicator (S), and propensity scores (pi.hat)
  population <- data
  population$Yhat <- yhat
  population$S <- coded
  population$pi.hat <- data$pi.hat

  # Calculate effect estimate and variance components grouped by treatment indicator (Z)
  est.part <- population %>% group_by(Z) %>%
    dplyr::summarise(
      # Calculate mean of predicted outcomes (Yhat) for the synthetic estimator
      Ybar = mean(Yhat),

      # Adjusted mean using model-assisted estimator
      adj = weighted.mean(Yobs[S == 1] - Yhat[S == 1], w = 1 / pi.hat[S == 1]),
      Ybar.adj = Ybar + adj,

      # Calculate the "true" variance of the estimate (tau.hat)
      N = n(),
      var.y = var(Yobs),
      var.resid = var(Yobs - Yhat),

      # Estimate the variance of the estimate (tau.hat) based on the subset of coded data
      var.y.coded = var(Yobs[S == 1]),
      n.z = sum(S),
      var.resid.coded = var(Yobs[S == 1] - Yhat[S == 1]),
      .groups = "drop"
    )

  # Calculate the effect estimate (difference in adjusted means)
  est <- diff(est.part$Ybar.adj)

  # Calculate the "true" variance of the effect estimate
  var <- sum(est.part$var.y / est.part$N) +
    sum((est.part$N - est.part$n.z) / est.part$N * (est.part$var.resid / est.part$n.z))

  # Calculate the estimated variance of the effect estimate based on the subset of coded data
  var.hat <- sum(est.part$var.y.coded / est.part$N) +
    sum((est.part$N - est.part$n.z) / est.part$N * (est.part$var.resid.coded / est.part$n.z))

  # Return a data frame with the effect estimate and its variances
  data.frame(est = est, SE = sqrt(var), SEhat = sqrt(var.hat))
}
