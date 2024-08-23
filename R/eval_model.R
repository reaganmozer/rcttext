#' Evaluate In-Sample and Out-of-Sample Performance of a Machine Learning Model
#'
#' This function calculates and records various performance measures for a machine learning model,
#' including in-sample (training) and out-of-sample (testing) metrics.
#'
#' @param yhat A numeric vector of predicted outcomes for the entire dataset.
#' @param Yobs A numeric vector of observed outcomes (ground truth) corresponding to the predictions in `yhat`.
#' @param coded A binary vector indicating whether a document was used for training (`1`) or testing (`0`).
#'
#' @return A data frame with the following performance measures for both in-sample and out-of-sample data:
#' \itemize{
#'   \item \code{ybar}: The mean of the predicted outcomes.
#'   \item \code{var.yhat}: The variance of the predicted outcomes.
#'   \item \code{R2}: The coefficient of determination (R-squared), indicating how well the predictions match the observed outcomes.
#'   \item \code{mse}: The mean squared error, representing the average squared difference between the predicted and observed outcomes.
#' }
#'
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' # Example usage with hypothetical data
#' yhat <- rnorm(100)
#' Yobs <- rnorm(100)
#' coded <- sample(c(0, 1), 100, replace = TRUE)
#' performance <- eval_model(yhat, Yobs, coded)
#' print(performance)
#' @export

eval_model <- function(yhat, Yobs, coded) {

  # Create a data frame with predictions, observed outcomes, and coding indicators
  tmp <- data.frame(
    yhat = as.vector(yhat),  # Predicted outcomes
    Yobs = Yobs,             # Observed outcomes
    coded = factor(coded, levels = 0:1, labels = c("test", "train"))  # Indicator for training (coded=1) or testing (coded=0)
  )

  # Calculate performance measures grouped by training and testing sets
  out <- tmp %>%
    dplyr::group_by(coded) %>%
    dplyr::summarise(
      ybar = mean(yhat),           # Mean of predicted outcomes
      var.yhat = var(yhat),        # Variance of predicted outcomes
      R2 = cor(yhat, Yobs)^2,      # Coefficient of determination (R-squared)
      mse = mean((Yobs - yhat)^2)  # Mean squared error
    )

  # Reshape the output to a wide format and calculate the overall variance of predicted outcomes
  out1 <- out %>%
    tidyr::pivot_wider(
      names_from = coded,
      values_from = c(ybar, var.yhat, R2, mse)
    ) %>%
    mutate(var.yhat = var(yhat))  # Calculate overall variance of predicted outcomes

  return(out1)
}
