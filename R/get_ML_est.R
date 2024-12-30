
#' Get All Estimates: Nominal (Oracle), Subset, Synthetic, and
#' Model-Assisted
#'
#' This function calculates and returns four types of estimates:
#' nominal (oracle), subset, synthetic, and model-assisted.
#'
#' @param dat A data frame containing the full dataset, including both
#'   observed and predicted outcomes.
#' @param coded A binary vector indicating which documents have been
#'   hand-coded (1 for coded, 0 for not coded).
#' @param yhat A numeric vector of predicted outcomes for the full
#'   dataset.
#'
#' @return A data frame containing the following estimates:
#' \itemize{
#'   \item \code{nominal.est}, \code{nominal.SE}, \code{nominal.SEhat}: Estimates based on the full hand-coded dataset.
#'   \item \code{subset.est}, \code{subset.SE}, \code{subset.SEhat}: Estimates based on the sample of hand-coded documents alone.
#'   \item \code{synth.est}, \code{synth.SE}, \code{synth.SEhat}: Oracle estimates based on predictions alone.
#'   \item \code{ML.est}, \code{ML.SE}, \code{ML.SEhat}: Model-assisted estimates that combine observed and predicted outcomes.
#' }
#'
#' @import dplyr
#' @export
#'
#' @examples
#' # Example usage with hypothetical data
#' dat <- data.frame(Yobs = rnorm(100), Z = sample(c(0, 1), 100, replace = TRUE))
#' coded <- sample(c(0, 1), 100, replace = TRUE)
#' yhat <- rnorm(100)
#' results <- get_ML_est(dat, coded, yhat)
#' print(results)
#' @export

get_ML_est <- function(dat, coded, yhat) {

  # Calculate model-assisted estimates based on predicted outcomes
  est.ML <- get.ests(yhat, coded = coded, data = dat)
  names(est.ML) <- paste0("ML.", names(est.ML))  # Prefix with 'ML.'

  # Estimate based on the full hand-coded dataset (nominal or oracle estimate)
  est.nom <- get.sub(coded = rep(1, nrow(dat)), dat)
  names(est.nom) <- paste0("nominal.", names(est.nom))  # Prefix with 'nominal.'

  # Estimate based on the subset of hand-coded documents only
  est.sub <- get.sub(coded, dat)
  names(est.sub) <- paste0("subset.", names(est.sub))  # Prefix with 'subset.'

  # Oracle estimate based on predictions alone (synthetic estimate)
  dat.pred <- dat %>% mutate(Yobs = yhat)
  est.synth <- get.sub(coded = rep(1, nrow(dat.pred)), dat.pred)
  names(est.synth) <- paste0("synth.", names(est.synth))  # Prefix with 'synth.'

  # Combine all estimates into a single data frame
  out <- cbind(est.nom, est.sub, est.synth, est.ML)

  # Return the combined estimates
  return(out)
}
