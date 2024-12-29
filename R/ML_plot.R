#' Create a Line Plot with Points using ggplot2
#'
#' @description This function creates a line plot with points using the ggplot2 package. It allows for extensive customization of aesthetics and layout.
#'
#' @param data A dataframe containing the data to be plotted.
#' @param x A string representing the column name for the x-axis.
#' @param y A string representing the column name for the y-axis.
#' @param color A string representing the column name to be used for coloring the lines and points.
#' @param group A string representing the column name to be used for grouping the lines.
#' @param ylim_min A numeric value representing the minimum limit for the y-axis.
#' @param ylim_max A numeric value representing the maximum limit for the y-axis.
#' @param x_lable A string for the label of the x-axis.
#' @param y_lable A string for the label of the y-axis.
#' @param fig_title A string for the title of the plot.
#' @param legend_title A string for the title of the legend.
#' @param legend.position A string representing the position of the legend in the plot (default is "bottom").
#'
#' @return A ggplot object representing the created plot.
#' @examples
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
#'
#' ## Define the percentages for training portions
#' percentages <- c(.20, 0.40, 0.60, 0.80)
#'
#' ## Loop through each percentage
#' best_mod <- 'rf'
#' random_forest_Scores = ML_iterations ( x = X, y = Y,
#'                                        n_iteration = n_iter,
#'                                        training_portions = percentages,
#'                                        trCon = control,
#'                                        preProc = preProc,
#'                                        n.tune = n_tune,
#'                                        model = best_mod,
#'                                        outcome = outcome )
#'
#' best_mod <- 'rrf'
#' regularized_rf_Scores = ML_iterations ( x = X, y = Y,
#'                                         n_iteration = n_iter,
#'                                         training_portions = percentages,
#'                                         trCon = control,
#'                                         preProc = preProc,
#'                                         n.tune = n_tune,
#'                                         model = best_mod,
#'                                         outcome = outcome )
#'
#' ## Evaluate the metrics
#' eval_random_forest_Scores = eval_metrics( random_forest_Scores,
#'                                           outcome = outcome )
#'
#' eval_regularized_rf_Scores = eval_metrics( regularized_rf_Scores,
#'                                            outcome = outcome )
#' ## Create data frames for data visualization
#' all_Scores = rbind( eval_random_forest_Scores,
#'                     eval_regularized_rf_Scores )
#'
#'
#' ## Create a line plot (Continuous outcomes)
#' ML_plot( data = all_Scores,
#'          x = "training", y = "rmse", color = "model", group = "model",
#'          ylim_min= 0.5, ylim_max= 0.7,
#'          x_lable = "Training Set (%)", y_lable = "RMSE",
#'          fig_title = "RMSE by Training set % for Different Models",
#'          legend_title = "", legend.position = "bottom" )
#'
#' @export


ML_plot = function (data = NULL,
                    x = NULL, y = NULL,
                    color = NULL,
                    group = NULL,
                    ylim_min= NULL,
                    ylim_max= NULL,
                    x_lable = NULL,
                    y_lable = NULL,
                    fig_title = NULL,
                    legend_title = "",
                    legend.position = "bottom" ) {

  ggplot2::ggplot(data, aes(x = .data[[x]], y = .data[[y]], color = .data[[color]], group = .data[[group]])) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::ylim(ylim_min, ylim_max) +
    ggplot2::labs(x = x_lable, y = y_lable, title = fig_title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = legend.position, panel.grid.major = ggplot2::element_blank()) +
    ggplot2::guides(color = ggplot2::guide_legend(title = legend_title))
}
