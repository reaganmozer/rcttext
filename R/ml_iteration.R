
#' Iterative Machine Learning with Performance Tracking
#'
#' Trains a machine learning model multiple times, splitting the data into
#' training and testing sets for each iteration. Evaluates and stores
#' performance metrics for each run, allowing for assessment of model
#' stability and generalization.
#'
#' @param x Matrix or data frame of predictor variables.
#' @param y Vector of outcome variable (continuous or categorical).
#' @param n_iteration Number of iterations (defaults to 1).
#' @param training_portion Proportion of data used for training (defaults to 0.8).
#' @param trCon Train control object for caret (optional).
#' @param preProc Preprocessing methods for caret (optional).
#' @param n.tune Number of tuning parameter combinations (optional).
#' @param model Machine learning model to use (e.g., "rf", "xgbTree").
#' @param grid Tuning grid for hyperparameter search (optional).
#' @param outcome Type of outcome variable: "continuous" or "categorical".
#' @return A list containing results for each iteration, including:
#'   * The trained model (`ML_model`)
#'   * Predicted values on the test set (`predicted_values`)
#'   * Values of predictor variables on the test set (`test_values`)
#'   * Performance metrics (RMSE, MAE, correlation, R-squared for continuous;
#'     confusion matrix, accuracy, kappa values for categorical)
#'   * Execution time for each iteration (`execution_time`)
#'
#' @details This function provides a standardized way to train and evaluate models
#' within a larger analysis, facilitating comparison across different models,
#' hyperparameters, or repeated runs. It handles both regression (continuous outcome)
#' and classification (categorical outcome) problems, providing appropriate evaluation
#' metrics for each.
#'
#' @seealso
#'  * `ML_iterations`: A wrapper function for performing multiple iterations of
#'  `ML_iteration` with different training proportions.
#'  * `caret::train`: The underlying function used for model training.
#'
#' @examples
#'
#' # Load texts
#' data("toy_reads")
#'
#' # Generate text features
#' feats = generate_features( toy_reads$text, meta=toy_reads,
#'                            sent = TRUE,
#'                            clean_features = TRUE,
#'                            read = c("Flesch","Flesch.Kincaid", "ARI"),
#'                            ld=c("TTR","R","K"),
#'                            ignore=c("ID"),
#'                            verbose = TRUE )
#'
#' # Preprocess the feature space to remove collinear features
#' # and features with near-zero variance
#' X_all = dplyr::select( feats,
#'                        -ID, -Q1, -Q2, -text, -more )
#' X_all = predict(caret::preProcess( X_all, method = c("nzv","corr"),
#'                                    uniqueCut=2, cutoff=0.95), X_all )
#' caret::findLinearCombos(X_all) # sanity check to make sure no redundant features
#'
#' # Transform all variables as numeric variables
#' X_all[] <- lapply(X_all,
#'                   function(x) if(is.character(x)) as.numeric(as.factor(x)) else x)
#'
#' # Extract outcome variables
#' all_Scores <- toy_reads$Q1
#'
#' ## Set parameters
#' X <- X_all
#' Y <- all_Scores
#' n_iter <- 2
#' n_tune <- 2
#' control <- caret::trainControl(method = 'cv')
#' preProc <- 'zv'
#' outcome <- 'continuous'
#' portion <- 0.7
#' best_mod <- 'rf'
#'
#' ## Run ML_iteration
#' random_forest_Score = ML_iteration (x = X, y = Y,
#'                                     n_iteration = n_iter,
#'                                     training_portion = portion,
#'                                     trCon = control,
#'                                     preProc = preProc,
#'                                     n.tune = n_tune,
#'                                     model = best_mod,
#'                                     outcome = outcome)
#'
#' @export

ML_iteration = function ( x,
                          y,
                          n_iteration = 1,
                          training_portion = 0.8,
                          trCon = NULL,
                          preProc = NULL,
                          n.tune = NULL,
                          model = NULL,
                          grid = NULL,
                          outcome = NULL) {

  doParallel::registerDoParallel(parallel::detectCores()-1)
  foreach::getDoParWorkers()

  training_index = caret::createDataPartition( y,
                                               times = n_iteration,
                                               p = training_portion,
                                               list = FALSE )

  # Create a list to store the results
  results_list <- list()

  # Initialize progress bar
  pb <- progress::progress_bar$new( format = "  [:bar] :percent :elapsed/:eta",
                                    total = n_iteration,
                                    clear = FALSE, width = 60 )

  for (i in 1:n_iteration) {
    tictoc::tic()  # Start timing

    x_train = x[training_index[, i], ]
    y_train = y[training_index[, i]]
    x_test = x[-training_index[, i], ]
    y_test = y[-training_index[, i]]

    if (outcome == "continuous") {
      pb$tick() # Update progress bar
      mod = caret::train( x = x_train,
                          y = y_train,
                          method = model,
                          preProcess = preProc,
                          tuneLength = n.tune,
                          trControl = trCon,
                          tuneGrid = grid )

      predicted_values <- predict(mod, x_test)
      test_x_values <- x_test

      rmse <- caret::RMSE(predict(mod, x_test), obs = y_test)
      mae <- caret::MAE(predict(mod, x_test), obs = y_test)
      correlation <- cor(predicted_values, y_test, use = "complete.obs")
      r_squared <- caret::R2(predict(mod, x_test), obs = y_test)
      time_taken <- tictoc::toc(log = TRUE, quiet = TRUE) # Stop timing and store the result

      # Save the results in a list
      result <- list(
        ML_model = mod,
        predicted_values = predicted_values,
        test_values = test_x_values,
        rmse = rmse,
        mae = mae,
        correlation = correlation,
        r_squared = r_squared,
        execution_time = time_taken)

      results_list[[i]] <- result
      Sys.sleep(0.1)

    } else if (outcome == "categorical") {
      pb$tick() # Update progress bar
      mod = caret::train( x = x_train,
                          y = y_train,
                          method = model,
                          preProcess = preProc,
                          tuneLength = n.tune,
                          trControl = trCon,
                          tuneGrid = grid )

      predicted_values <- predict(mod, x_test)
      test_x_values <- x_test
      conf.mat <- confusionMatrix(as.factor(predicted_values), as.factor(y_test))
      focal_values <- conf.mat$overall
      kappa_values <- psych::cohen.kappa(data.frame(y_test, predicted_values))
      table <- conf.mat$table
      time_taken <- tictoc::toc(log = TRUE, quiet = TRUE) # Stop timing and store the result

      # Save the results in a list
      result <- list(
        ML_model = mod,
        predicted_values = predicted_values,
        test_values = test_x_values,
        confusionmatrix_result = conf.mat,
        focal_values = focal_values,
        kappa_values = kappa_values,
        confusionmatrix = table,
        execution_time = time_taken)

      results_list[[i]] <- result
      Sys.sleep(0.1)
    }
    else {
      stop("Please specify the type of outcome variables: 'categorical' or 'continuous'")
    }
  }
  doParallel::stopImplicitCluster()

  # Return the list of results
  return(results_list)
}

#' Iterative Machine Learning Across Multiple Training Set Sizes
#'
#' Trains and evaluates a specified machine learning model iteratively,
#' using different proportions of the dataset for training in each iteration.
#' This function facilitates the exploration of how model performance
#' varies with changes in the amount of training data. Hyperparameter
#' tuning can be optionally incorporated to optimize the model within each
#' iteration.
#'
#' @rdname ML_iteration
#' @param x Matrix or data frame of predictor variables.
#' @param y Vector of outcome variable (continuous or categorical).
#' @param n_iteration Number of times to repeat model training and evaluation
#' for each training set size (defaults to 1).
#' @param training_portions A vector of proportions (between 0 and 1)
#' indicating the fraction of data to be used for training in each iteration.
#' @param trCon Train control object for caret (optional).
#' @param preProc Preprocessing methods for caret (optional).
#' @param n.tune Number of tuning parameter combinations (optional).
#' @param model Machine learning model to use (e.g., "rf", "xgbTree").
#' @param grid Tuning grid for hyperparameter search (optional).
#' @param outcome Type of outcome variable: "continuous" or "categorical".
#'
#' @return A nested list, where the first level corresponds
#' to different training set sizes and the second level contains results
#' for each iteration within that training set size.
#' Each iteration's results include:
#'   * The trained model (`ML_model`)
#'   * Predicted values on the test set (`predicted_values`)
#'   * Values of predictor variables on the test set (`test_values`)
#'   * Performance metrics (RMSE, MAE, correlation, R-squared for continuous;
#'     confusion matrix, accuracy, kappa values for categorical)
#'   * Execution time for each iteration (`execution_time`)
#'
#' @details
#' This function helps you understand how your chosen model performs with
#' varying amounts of training data, which is crucial for assessing its
#' potential in real-world scenarios with limited data. The inclusion of
#' hyperparameter tuning can further optimize the model's performance
#' for each training set size.
#'
#' @seealso
#'  * `ML_iteration`: The core function performing a single iteration of model
#'  training and evaluation.
#'  * `caret::train`: The underlying function used for model training.
#'
#' @examples
#'
#' # Load texts
#' data("toy_reads")
#'
#' # Generate text features
#' feats = generate_features( toy_reads$text, meta=toy_reads,
#'                            sent = TRUE,
#'                            clean_features = TRUE,
#'                            read = c("Flesch","Flesch.Kincaid", "ARI"),
#'                            ld=c("TTR","R","K"),
#'                            ignore=c("ID"),
#'                            verbose = TRUE )

#' # Preprocess the feature space to remove collinear features
#' # and features with near-zero variance
#' X_all = dplyr::select( feats,
#'                        -ID, -Q1, -Q2, -text, -more )
#' X_all = predict(caret::preProcess( X_all, method = c("nzv","corr"),
#'                                    uniqueCut=2, cutoff=0.95), X_all )
#' caret::findLinearCombos(X_all) # sanity check to make sure no redundant features
#'
#' # Transform all variables as numeric variables
#' X_all[] <- lapply(X_all,
#'                   function(x) if(is.character(x)) as.numeric(as.factor(x)) else x)
#'
#' # Extract outcome variables
#' all_Scores <- toy_reads$Q1
#'
#' ## Set parameters
#' X <- X_all
#' Y <- all_Scores
#' n_iter <- 2
#' n_tune <- 2
#' control <- caret::trainControl(method = 'cv')
#' preProc <- 'zv'
#' outcome <- 'continuous'
#' best_mod <- 'rf'
#'
#' ## Define the percentages for training portions
#' percentages <- c(.20, 0.40, 0.60, 0.80)
#'
#' ## Loop through each percentage
#' random_forest_Scores = ML_iterations (x = X, y = Y,
#'                                       n_iteration = n_iter,
#'                                       training_portions = percentages,
#'                                       trCon = control,
#'                                       preProc = preProc,
#'                                       n.tune = n_tune,
#'                                       model = best_mod,
#'                                       outcome = outcome)
#'
#' @export

ML_iterations = function ( x,
                           y,
                           n_iteration = 1,
                           training_portions = 0.8,
                           trCon = NULL,
                           preProc = NULL,
                           n.tune = NULL,
                           model = NULL,
                           grid = NULL,
                           outcome = NULL
) {

  percentages <- training_portions
  results <- list()

  for (percent in percentages) {
    print( paste0( "training set ", percent * 100,"% - Start" ))
    training <- paste0( "train_", percent * 100, "_percent" )
    results[[training]] <- ML_iteration( x = X, y = Y,
                                         n_iteration = n_iteration,
                                         training_portion = percent,
                                         trCon = trCon,
                                         preProc = preProc,
                                         n.tune = n.tune,
                                         model = model,
                                         outcome = outcome )
  }
  # Return the list of results
  return(results)
}
