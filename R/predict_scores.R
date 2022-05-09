#' Extract predictions from a fitted text scoring model.
#'
#' This function computes the predicted scores for a collection of documents based on the
#' results of a trained ensemble learner.
#'
#' @param fit a model or list of models to use for prediction
#' @param newdata an optional data frame or matrix of predictors
#' @param na.action the method for handling missing data
#' @param ... additional arguments to pass to \code{predict.train}
#' @return A vector of predictions
#' @export
#'


predict_scores = function(fit, newdata, na.action=na.omit, ...){


}
