#' Extract predictions from a fitted text scoring model.
#'
#' This function calculates predicted scores for all documents based on fitted models. It handles both in-sample and out-of-sample predictions for different groups.
#'
#' @param fit A list containing fitted models for two groups (typically treatment and control). The list should include `mod0` for group 0 and `mod1` for group 1, along with a binary vector `coded` indicating which documents are part of the coded subset.
#' @param X A data frame or matrix of predictor variables used for making predictions.
#' @param Z A binary vector indicating group membership (e.g., 0 for control, 1 for treatment).
#'
#' @return A numeric vector of predicted scores for all documents.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' # Example usage with hypothetical data
#' fit <- list(mod0 = lm(Y ~ X1 + X2, data = data0), mod1 = lm(Y ~ X1 + X2, data = data1), coded = c(1, 0, 1, 0, 1))
#' X <- data.frame(X1 = rnorm(100), X2 = rnorm(100))
#' Z <- sample(c(0, 1), 100, replace = TRUE)
#' yhat <- predict_scores(fit, X, Z)
#' print(yhat)
#'
#' @export

predict_scores = function(fit, newdata, na.action=na.omit, ...){


}

# Calculate predicted scores for all documents
predict_scores = function(fit,  X, Z){

  X0 =  subset(X, Z==0)
  X1 =  subset(X, Z==1)

  yhat0 = predict(fit$mod0, X0)
  yhat1 = predict(fit$mod1, X1)


  yhat.pred = rep(NA, nrow(X))
  yhat.pred[Z==0] = yhat0
  yhat.pred[Z==1] = yhat1


  yhat.out0 = as.data.frame(fit$mod0$pred %>% arrange(rowIndex))$pred
  yhat.out1 = as.data.frame(fit$mod1$pred %>% arrange(rowIndex))$pred


  # Use OOS predictions for coded documents
  yhat.all = yhat.pred
  yhat.all[Z==0 & fit$coded==1]=yhat.out0
  yhat.all[Z==1 & fit$coded==1]=yhat.out1


  yhat.all = as.numeric(yhat.all)
  return(yhat.all)
}
