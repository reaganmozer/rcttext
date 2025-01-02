


#' Train machine learners on a single (unlabeled) dataset.
#'
#' Train a bunch of machine learners, along with an ensemble made of
#' those learners, and return the list of fit models.
#'
#' @param x A matrix of features
#' @param y A vector of outcomes
#' @param n.tune The number of tuning parameters to search over
#' @param preProc A pre-processing list
#' @param bounds The bounds of the outcome variable
#' @param include_BART Should we include BART in the ensemble?
#' @param include_ensemble Should we include the ensemble in the list
#'   of models returned?  If TRUE, will return vanilla list, not a
#'   caretList, of models.
#' @param methods A character vector of methods to use.  Special
#'   values of "small" will use a small, hand-picked list of defaults.
#'   "full" will use a larger list of defaults.
#' @param verbose Should we print out progress?
#' @return A list of caret 'train' objects (or ensamble equivilents)
#'   that can be used to predict on new data.
#' @export
train_models = function( x, y, n.tune=3, preProc=NULL, bounds=NULL,
                         methods = "small",
                         include_BART = TRUE,
                         include_ensemble = TRUE,
                         verbose = TRUE ) {

  doParallel::registerDoParallel(parallel::detectCores()-1)
  foreach::getDoParWorkers()


  ind = caret::createResample(y, times=5)
  control = caret::trainControl(method="cv", number=5, index=ind,
                                savePredictions="final", allowParallel=T,
                                predictionBounds = bounds)

  # Fit our "ML" models

  stopifnot( !is.null( methods ) && is.character(methods) )
  if ( length(methods) == 1 &&  methods == "full" ) {
    methods = c("bstTree", "cforest","cubist",
                "glmnet", "knn", "pcr",
                "rf", "rpart1SE", "RRFglobal",
                "svmPoly","svmRadial", "treebag")
  } else if ( length(methods) == 1 && methods == "small" ) {

    methods = c("bstTree", "cforest","cubist",
                "glmnet", "knn", "pcr" )
  }

  if ( verbose ) {
    cmeth = paste( methods, collapse=", " )
    cat( glue::glue( "Making caret List with methods {cmeth}" ) )
    cat( "\n")
  }

  # TODO: What is this line for?  And why gbm specific?
  methods.tl = list(gbm = caretEnsemble::caretModelSpec(method="gbm",verbose=F))

  mods = caretEnsemble::caretList(x=as.matrix(x), y=y,trControl=control, preProcess=preProc,
                                  methodList=methods,tuneLength=n.tune, tuneList=methods.tl)
  all.mods = mods

  if ( verbose ) {
    cat( "trainControl\n" )
  }
  c0 = caret::trainControl(method="cv", number=5, index=ind, predictionBounds=bounds,
                           savePredictions="final",allowParallel=F)

  if ( include_BART ) {
    if ( verbose ) {
      cat( "bart\n" )
    }
    bart = caretEnsemble::caretList(x=as.matrix(x), y=y,  preProcess=preProc,
                                    methodList="bartMachine", trControl=c0,
                                    verbose=FALSE, serialize=TRUE,
                                    tuneGrid=data.frame(num_trees=c(50,100), k=2, alpha=0.95,
                                                        beta=2, nu=3))



    all.mods = c(mods, bart)
  }

  if ( include_ensemble ) {
    if ( verbose ) {
      cat( "stack\n" )
    }
    tc.new=caret::trainControl(predictionBounds=bounds,
                               method="cv",number=5)

    stack = caretEnsemble::caretStack(all.mods, trControl=tc.new, tuneLength=n.tune*2)
  }

  doParallel::stopImplicitCluster()

  if ( include_ensemble ) {
    fit = c( list( stack = stack ), all.mods )
    #names(fit)=c("all.mods","stack")
    return(fit)
  } else {
    return( all.mods )
  }
}




#' Add predictions to a meta dataframe
#'
#' Given a set of models (as a list) add predictions from model to a
#' passed dataframe
#'
#' @param models List of models from, e.g., train_models
#' @param features A dataframe of features that correspond to the
#'   features the models were trained on (but possibly for different
#'   data).
#' @param meta Dataframe to add predictions to.  If NULL, just return
#'   a dataframe of the predictions.
#' @export
generate_predictions = function( models, features, meta = NULL, prefix = "mod_" ) {
  stopifnot( is.list(models) )
  stopifnot( is.matrix(features) || is.data.frame(features) )
  stopifnot( is.null(meta) || is.data.frame(meta) )

  cc = complete.cases(features) & !apply(features, 1, function(x) any(is.infinite(x)))
  if ( any(!cc) ) {
    warning( paste0( "Skipping ", sum(!cc), " rows with missing or infinite values." ) )
    features = features[cc,]
    if ( !is.null( meta ) ) {
      meta = meta[cc,]
    }
  }

  pds = predict( models, features ) %>%
    dplyr::bind_cols() %>%
    as.data.frame()

  # colnames(pds) = tm::removeNumbers(colnames(pds))

  colnames(pds) = paste0( prefix, names(models) )

  if ( is.null(meta) ) {
    return(pds)
  } else {
    stopifnot( nrow(meta) == nrow(pds) )
    return( cbind(meta, pds) )
  }
}
