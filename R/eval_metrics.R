
#' Evaluate Metrics for Machine Learning Iterations
#'
#' This function calculates evaluation metrics for the results of
#' machine learning iterations. Depending on the type of outcome variable
#' (continuous or categorical), the function computes different sets of metrics
#' and returns them in a summarized format.
#'
#' @param result A list of results from `ML_iteration` or `ML_iterations`.
#' Each element of the list should be a list containing
#' the results of a single iteration, including the calculated metrics.
#' @param outcome Type of outcome variable: either "continuous"
#' for continuous outcomes or "categorical" for categorical outcomes.
#' @return A data frame containing the evaluation metrics.
#' For continuous outcomes, the metrics include:
#'   - `rmse`: Root Mean Squared Error
#'   - `r_squared`: R-squared value
#'   - `mae`: Mean Absolute Error
#'
#'   For categorical outcomes, the metrics include:
#'   - `accuracy`: Accuracy
#'   - `accuracy_ll`: Lower limit of the accuracy confidence interval
#'   - `accuracy_ul`: Upper limit of the accuracy confidence interval
#'   - `uwk`: Unweighted kappa
#'   - `qwk`: Quadratic weighted kappa
#'
#'@examples
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
#' ## Evaluate the metrics
#' eval_metrics(random_forest_Scores, outcome = outcome)
#'
#'@export
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
