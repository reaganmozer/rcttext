
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
#' \dontrun{}
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


#' @rdname ML_iteration
#' Iterative Machine Learning Across Multiple Training Set Sizes
#'
#' Trains and evaluates a specified machine learning model iteratively,
#' using different proportions of the dataset for training in each iteration.
#' This function facilitates the exploration of how model performance
#' varies with changes in the amount of training data. Hyperparameter
#' tuning can be optionally incorporated to optimize the model within each
#' iteration.
#'
#'
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
#' \dontrun{}
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

# Provide average values of evaluation metrics ----

eval_metrics = function (result = NULL,
                         outcome = NULL) {
  if (outcome == "continuous") {
    ## Set the number of iterations
    n_iter = length(result[[1]])
    percentages = length(result)

    ## Create empty data frames
    rmse_df = as.data.frame( matrix( nrow = n_iter,
                                     ncol = percentages ))
    r_squared_df = as.data.frame( matrix( nrow = n_iter,
                                          ncol = percentages ))
    mae_df = as.data.frame( matrix( nrow = n_iter,
                                    ncol = percentages ))

    ## Extract relevant results
    for (i in 1:percentages) {
      colnames(rmse_df)[i] = names(result[i])
      colnames(r_squared_df)[i] = names(result[i])
      colnames(mae_df)[i] = names(result[i])
      for (j in 1:n_iter) {
        #rmse
        rmse_df[j,i] = result[[i]][[j]]$rmse

        #r_squared
        r_squared_df[j,i] = result[[i]][[j]]$r_squared

        #mae
        mae_df[j,i] = result[[i]][[j]]$mae
      }
    }

    all_results = data.frame( rmse = ( colMeans( rmse_df )),
                              r_squared = ( colMeans( r_squared_df )),
                              mae = ( colMeans( mae_df )))
    # Return the list of results
    return(all_results)
  }
  else if (outcome == "categorical") {
    ## Set the number of iterations
    n_iter = length(result[[1]])
    percentages = length(result)

    ## Create empty data frames
    accuracy_df = as.data.frame( matrix( nrow = n_iter,
                                         ncol = percentages ) )
    accuracy_ll = as.data.frame( matrix( nrow = n_iter,
                                         ncol = percentages ) )
    accuracy_ul = as.data.frame( matrix( nrow = n_iter,
                                         ncol = percentages ) )
    uwk_df = as.data.frame( matrix( nrow = n_iter,
                                    ncol = percentages ) )
    qwk_df = as.data.frame( matrix( nrow = n_iter,
                                    ncol = percentages ) )

    ## Extract relevant results
    for (i in 1:percentages) {
      colnames(accuracy_df)[i] = names(result[i])
      colnames(accuracy_ll)[i] = names(result[i])
      colnames(accuracy_ul)[i] = names(result[i])
      colnames(uwk_df)[i] = names(result[i])
      colnames(qwk_df)[i] = names(result[i])
      for (j in 1:n_iter) {
        # Accuracy
        accuracy_df[j,i] = result[[i]][[j]]$focal_values[1]

        # Accuracy LL
        accuracy_ll[j,i] = result[[i]][[j]]$focal_values[3]

        # Accuracy UL
        accuracy_ul[j,i] = result[[i]][[j]]$focal_values[4]

        # Unweighted kappa (uwk)
        uwk_df[j,i] = result[[i]][[j]]$focal_values[2]

        # Quadratic weighted kappa (qwk)
        qwk_df[j,i] = result[[i]][[j]][["kappa_values"]][["weighted.kappa"]]
      }
    }

    all_results = data.frame( accuracy = ( colMeans( accuracy_df )),
                              accuracy_ll = ( colMeans( accuracy_ll )),
                              accuracy_ul = ( colMeans( accuracy_ul )),
                              uwk = ( colMeans( uwk_df )),
                              qwk = ( colMeans( qwk_df )))

    # Return the list of results
    return(all_results)
  }
}
