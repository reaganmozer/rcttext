
#' best_ML Function
#'
#' This function evaluates multiple machine learning models using specified
#' resampling methods and returns the best performing model based on R-squared.
#'
#' @param features A data frame of feature variables.
#' @param outcome A vector or data frame containing the outcome variable.
#' @param num Either the number of folds (for cross-validation) or
#'   the number of resampling iterations.
#' @param por The proportion of data to be used for training,
#'   with the remainder used for testing (default is 0.8).
#' @param method The resampling method to be used. Options include "boot",
#'   "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV",
#'   "LGOCV", "none", "oob", "adaptive_cv", "adaptive_boot", and "adaptive_LGOCV".
#'   Default is "cv".
#' @param mlList A vector of machine learning methods to be used.
#'   Options include "rf" (random forest) and "xgbTree" (XGBoost tree).
#'   Default is c("rf", "xgbTree").
#'
#' @return A list containing:
#' \item{mean_performance}{A data frame of average performance metrics (e.g., R-squared) for each model.}
#' \item{best_model}{The name of the best-performing model based on the highest R-squared value.}
#'
#' @import caret
#' @import caretEnsemble
#' @import dplyr
#' @export
#'
#' @examples
#' # Load necessary libraries
#'
#' # Simulate example data
#' set.seed(123)
#' features <- data.frame(
#'   x1 = rnorm(100),
#'   x2 = rnorm(100),
#'   x3 = rnorm(100)
#' )
#' outcome <- rnorm(100)
#'
#' # Apply the best_ML function
#' result <- best_ML(features = features, outcome = outcome, num = 3, por = 0.8,
#'                    method = "cv", mlList = c("rf", "xgbTree"))
#'
#' # View the results
#' print(result)
#'
#' @export

best_ML <- function( features, outcome, num = 3, por = 0.8,
                     method = c( "boot", "boot632", "optimism_boot",
                                 "boot_all", "cv", "repeatedcv",
                                 "LOOCV", "LGOCV", "none", "oob",
                                 "adaptive_cv", "adaptive_boot",
                                 "adaptive_LGOCV" ),
                     mlList = c( "rf", "xgbTree" )) {

  # Match the specified method with available options
  method <- match.arg( method, choices = c( "cv","boot", "boot632",
                                            "optimism_boot", "boot_all",
                                            "repeatedcv","LOOCV", "LGOCV",
                                            "none", "oob","adaptive_cv",
                                            "adaptive_boot", "adaptive_LGOCV" ))

  # Combine outcome and features
  df <- cbind( features, outcome )

  # Create data partition
  id <- caret::createDataPartition( y = df$outcome,
                                    p = por,
                                    list = FALSE )

  # Split into training and testing sets
  df_train <- df[id, ]
  df_test <- df[-id, ]

  # Set up trainControl
  control <- caret::trainControl( method = method,
                                  number = num,
                                  savePredictions = "final",
                                  index = createResample( df_train$outcome, num ),
                                  allowParallel = TRUE )

  # Train multiple models
  models <- caretEnsemble::caretList( outcome ~ .,
                                      data = df_train,
                                      trControl = control,
                                      methodList = mlList )

  # Extract results from models
  list_of_results <- lapply( mlList, function(x) {models[[x]]$resample} )

  # Convert results to data frame
  df_results <- dplyr::bind_rows( list_of_results )

  # Add model names to results
  df_results <- df_results %>%
    dplyr::mutate( Model = lapply( mlList, function(x) {rep(x, num)}) %>% unlist() )

  # Remove 'Resample' column
  ML_Results <- df_results %>% dplyr::select( -Resample )

  # Calculate average performance for each model
  average_performances <- caretEnsemble::aggregate( . ~ Model, data = ML_Results, mean )

  # Identify the best model based on R-squared
  best_model <- average_performances[which.max(average_performances$Rsquared), "Model"]

  result <- list(
    mean_performance = average_performances,
    best_model = best_model)
  # Save the results in a list

  return(result)
}
