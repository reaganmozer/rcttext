



#' For each column of x, conduct an analysis of impact of MORE
#' intervention on feature represented by that column.
#'
#' Adjust all tests with FDR at end.  For an example function for
#' analysis_function see simple_RCT_analysis.
#'
#' @param data Dataframe of the meta information and features.  Will
#'   calculate impacts on all columns of data that are not specified
#'   by ignore.
#' @param ignore Character vector of column names to ignore in the
#'   analysis.  Or list of column indices.
#' @param meta Extra information that are not text features to be
#'   analyzed (e.g. treatment assignment indicator) that should be
#'   given to each analysis for each feature (this is a dataframe)
#' @param analysis_function Function to apply to each column of data.
#'   Should return a one-row tibble with columns such as Estimate, SE,
#'   t, p, CI_l, CI_h.  Function is of form f( feature, data, ... )
#'   with feature being a vector of values, with length equal to
#'   number of rows of data.  Function results MUST return a column
#'   'p.value' of the p.value of the test.
#' @param standardize Logical.  If TRUE, standardize the effect sizes
#'   in new columns of output with suffix "_std".  If "pool", pool the
#'   standard deviations of the two treatment groups to get the
#'   standardize metric.
#' @param mcp character string specifying the correction method to be
#'   applied to adjust for multiple comparisons. Defaults to fdr
#'   adjustments. See \link{p.adjust} for available adjustment
#'   methods.  NULL means no adjustment.
#' @param planned_features Character vector of features that are
#'   planned for assessment.  They will never be screened out,
#'   regardless of their p-value.  Multiple comparison is done within
#'   group of planned and unplanned features.
#' @param alpha Significance level for hypothesis tests.
#' @param ... Additional arguments to pass to analysis_function.
#'
#' @seealso simple_RCT_analysis
#'
#' @export
impacts_on_features <- function( data,
                                 meta = NULL,
                                 ignore = NULL,
                                 analysis_function = simple_RCT_analysis,
                                 mcp = "fdr",
                                 planned_features = NULL,
                                 standardize = TRUE,
                                 alpha = 0.05,
                                 ... ) {
  stopifnot( !is.null(data) )
  stopifnot( is.data.frame(data) || is.matrix(data) )

  # Separate out features and the rest of the data
  if ( !is.null( ignore ) ) {
    x = data %>%
      dplyr::select( -any_of( ignore ) )
    meta2 = data %>%
      dplyr::select( any_of( ignore ) )

    if ( is.null(meta) ) {
      meta = meta2
    } else {
      stopifnot( is.data.frame(meta) )
      stopifnot( nrow( meta ) == nrow(meta2) )
      com_names = intersect( colnames(meta), colnames(meta2) )
      if ( length( com_names ) > 0 ) {
        # drop those columns in meta2
        meta2 <- meta2 %>%
          dplyr::select( -any_of( com_names ) )
      }
      meta = bind_cols( meta, meta2 )
    }
  } else {
    x = data
  }

  res <- map( x, analysis_function, data=meta, ... ) %>%
    list_rbind()

  res <- res %>%
    mutate( feature = colnames(x) ) %>%
    relocate( feature )

  # Effect size conversion
  if ( standardize != FALSE ) {
    res <- standardize_impact_table( res, standardize )
  }

  # Multiple testing correction
  if ( is.null( planned_features ) ) {
    planned_features = character(0)
  }
  res$planned=1*(res$feature %in% planned_features)

  if ( !is.null( mcp ) ) {
    stopifnot( "p.value" %in% colnames(res) )
    res$p.adj = NA
    res$p.adj[ res$planned==1 ] = p.adjust( res$p.value[ res$planned==1 ], mcp )
    res$p.adj[ res$planned==0 ] = p.adjust( res$p.value[ res$planned==0 ], mcp )
  }

  return(res)
}

standardize_impact_table <- function( res, standardize = TRUE ) {

  # If already standardized, bail
  if ( "estimate_std" %in% colnames(res) ) {
    return(res)
  }

  if ( standardize == "pool" ) {
    res$scale = sqrt( (res$Grp_1_sd^2 + res$Grp_0_sd^2) / 2 )
  } else {
    res$scale = res$Grp_0_sd
  }

  res <- res %>%
    mutate(across(any_of(c("estimate", "std.error", "conf.low", "conf.high")),
                  ~ .x / scale, .names = "{.col}_std"))

  res
}


#' Default analysis_function for impacts_on_features
#'
#' This is the default analysis function to use in impacts_on_features.
#'
#' @param feature Vector of values for the feature of interest.
#' @param formula Formula to use in the analysis of form ~ Z + X1,
#'   where the first argument has to be the treatment indicator.
#' @param data Dataframe with the feature and other variables.
#' @param ... Additional arguments to pass to lm_robust.
#'
#' @importFrom estimatr lm_robust
#'
#' @seealso impacts_on_features
#' @export
simple_RCT_analysis <- function( feature, formula, data, ... ) {
  stopifnot( !is.null(feature) )
  stopifnot( nrow(data) == length(feature) )

  data$.feature = feature
  formula <- update( as.formula( formula ), .feature ~ . )

  rhs_vars <- attr(terms(formula), "term.labels")
  first_var <- rhs_vars[1]
  stopifnot( first_var %in% colnames(data) )

  mod = estimatr::lm_robust( formula, data=data, ... )

  res <- broom::tidy( mod ) %>%
    filter( term == first_var ) %>%
    dplyr::select( -term, -outcome )

  # Add in subgroup means and standard deviations, if binary treatment
  uu = unique( data[[first_var]] )
  if ( length( uu ) == 2 ) {
    nm1 = paste0( "Grp_", uu[1] )
    nm2 = paste0( "Grp_", uu[2] )
    res[nm1] = mean( data$.feature[ data[[first_var]] == uu[1] ] )
    res[nm2] = mean( data$.feature[ data[[first_var]] == uu[2] ] )
    res[paste0( nm1, "_sd" )] = sd( data$.feature[ data[[first_var]] == uu[1] ] )
    res[paste0( nm2, "_sd" )] = sd( data$.feature[ data[[first_var]] == uu[2] ] )
  }

  res
}




#' Given text from a randomized trial with a binary treatment, this
#' function computes estimates for the average treatment effect with
#' respect to an array of text-based outcomes
#'
#'
#' @param x A character vector of text documents or a feature matrix
#' @param Z Indicator for treatment assignment.
#' @param adj (optional) character vector or named list of variables
#'   in the data matrix to adjust for when estimating treatment
#'   impacts.
#' @param data A \code{data.frame} of subject-level identifiers,
#'   demographic variables, group membership, and/or other
#'   pre-treatment covariates.
#' @param design For multi-site and cluster randomized experiments, a
#'   named list of vectors containing site IDs and/or cluster IDs.
#' @param wts Sampling weights for documents.  Assumed uniform if
#'   null.
#' @return A model object for estimating treatment impact across an
#'   array of features.

#' @export

textfx <- function(x, Z, adj=NULL, data,
                   wts = NULL,
                   design=list(siteID=NULL, clusterID=NULL)){



}


#' Estimates the average treatment effect on the frequency and
#' prevalence of a list of specified terms and phrases.
#'
#' @param x A character vector of text documents or a feature matrix
#' @param Z Indicator for treatment assignment.
#' @param terms Terms and phrases to evaluate
#' @param ... optional parameters passed to \link{quanteda::tokens()}
#'
#' @return A vector showing the frequency and prevalence of the
#'   specified terms within each treatment group and results of the
#'   hypothesis test comparing prevalence across groups.

#' @export

textfx_terms = function(x, Z, terms, ...){

  # Determine maximum number of ngrams to search based on the input terms
  ngrams.terms = stringi::stri_count_words(terms)
  max.ngrams = 1:max(ngrams.terms)

  # Convert ngrams to quanteda structure with underscores
  terms = gsub(" ", "_", terms, fixed=T)

  dfm = quanteda::dfm(quanteda::tokens_ngrams(quanteda::tokens(x,...), n=max.ngrams))
  dfm.terms = as.matrix(quanteda::dfm_match(dfm, terms))

  out = data.frame(Z=Z, dfm.terms, tot=rowSums(dfm.terms))
  out = out %>% group_by(Z) %>%
    summarise(n=n(), termfreq=sum(tot), docfreq=sum(tot>0), prop.docs=docfreq/n) %>%
    arrange(desc(Z))

  # Hypothesis test for difference in proportions between groups
  test = prop.test(x=out$docfreq, n=out$n)

  out1 = out %>% pivot_wider(names_from=Z, values_from=!Z) %>%
    mutate( diff = test$estimate[1]-test$estimate[2],
            p.value = test$p.value,
            LL = test$conf.int[1],
            UL = test$conf.int[2])

  return(out1)

}


