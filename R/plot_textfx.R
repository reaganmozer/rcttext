
#' Plot the results from an impact analysis with text outcomes
#'
#' This function provides a visualization of the set of textual
#' features found to differ systematically between treatment and
#' control groups.
#'
#' The `out` object should have columns named `feature`, `estimate`,
#' `conf.low`, `conf.high`, and `p.adj` (or `p.value`) for the
#' estimated effect, confidence interval bounds, and adjusted p-value
#' for each feature.  If the `standardize` argument is not FALSE, the
#' function will standardize estimates based on columns of `Grp_0_sd`
#' and `Grp_1_sd` (if pooling).
#'
#' @importFrom plotrix plotCI
#' @import scales
#' @import dplyr
#'
#' @param out a model object output from \code{impacts_on_features()}
#' @param feature Name of the column of feature names.
#' @param alpha the threshold for determining statistical significance
#' @param cols should effects be colored by direction (red for
#'   negative impacts, blue for positive impacts)
#' @param group (optional) should effects be grouped by category
#'   (e.g., higher-level summary measures, linguistic features, etc.)
#' @param standardize (optional) should effects be standardized by the
#'   pooled standard deviation of the feature across treatment and
#'   control groups.  Can be TRUE, FALSE, or "pool".
#' @param xlim (optional) bounds for x-axis
#' @param ... additional arguments passed to plot
#' @export

plot_textfx= function( out,
                       alpha=0.05, cols=FALSE, group=TRUE, standardize = TRUE,
                       xlim=NULL, ...){


  if ( standardize != FALSE ) {
    out = standardize_impact_table( out, standardize = standardize )
    R = nrow(out)
    # Drop features where there is no variation in the feature
    out <- out %>%
      dplyr::filter( scale > 0 )
    if ( nrow(out) < R ) {
      warning( "Features dropped due to no variation in tx or co group" )
    }

    stopifnot( all( c( "feature", "estimate_std",
                       "conf.high_std", "conf.low_std" ) %in% colnames(out)) )

  } else {
    stopifnot( all( c( "feature", "estimate",
                       "conf.high", "conf.low" ) %in% colnames(out)) )

    out$estimate_std = out$estimate
    out$conf.high_std = out$conf.high
    out$conf.low_std = out$conf.low
  }



  out$ord = rep(3,nrow(out))
  if (group){
    out$ord[out$feature %in% c("Analytic","Authentic","Clout","Tone")] = 1
    out$ord[out$feature %in% c("WC","WPS","TTR","xxx","Sixltr",
                                 "Flesch.Kincaid", "Flesch","XXX",
                                 "R","ARI")] = 2
  }
  dimnames = get_dimnames()
  out = merge(out, dimnames, by="feature",all.x=T)
  out$fname[is.na(out$fname)]=out$feature[is.na(out$fname)]
  out = dplyr::arrange(out, desc(ord), desc(fname))

  ptcols="gray"
  if (cols){ptcols= ifelse(out$est>0,"blue","red")}
  if (!is.null(xlim)){
    xl=xlim[1]
    xr=xlim[2]
  } else {
    xl=min(out$conf.low_std)-0.1
    xr=max(out$conf.high_std)+0.1
  }

  xlabel = "Standardized difference in means (95% CI)"

  plotrix::plotCI(x=out$estimate_std, y=1:nrow(out),
                  ui=out$conf.high_std, li=out$conf.low_std,
                  err="x",yaxt="n",ylab="",xlab=xlabel,
                  pch=NA,scol="white",xlim=c(xl-0.05,xr+0.05),xaxt="n",...)
  axis(side=1,at=seq(xl, xr, by=0.2),labels=sprintf("%0.1f",seq(xl,xr,0.2)))
  abline(v=0,lty=2)
  if (length(unique(out$ord))>2){
    ind=c(min(which(out$ord==1)),min(which(out$ord==2)))-0.5
    abline(h=ind[1],col="darkgray",lty=3,lwd=2)
    abline(h=ind[2],col="darkgray",lty=3,lwd=2)
  }


  linecols = ifelse(out$p.adj<=alpha, 1,0.5)
  ptwt = ifelse(out$p.adj<=alpha, 0.75, 0.1)
  plotrix::plotCI(x=out$estimate_std, y=1:nrow(out), ui=out$conf.high_std, li=out$conf.low_std,
                  err="x",add=T,scol=alpha("black",linecols), col=alpha("black",linecols),
                  pch=21, pt.bg=alpha(ptcols,ptwt), cex=1.3,lwd=1.25)

  which.sigs = out$p.adj<=alpha
  y1 = (1:nrow(out))[!which.sigs]
  y2 = (1:nrow(out))[which.sigs]


  if ( length(y1) > 0 ) {
    axis(side=2,at=y1,labels=out$fname[!which.sigs],las=2,cex.axis=0.875,tcl=-0.25)
  }
  if ( length(y2) > 0 ) {
    axis(side=2,at=y2,labels=out$fname[which.sigs],las=2,cex.axis=0.95,tcl=-0.25,
         font=2)
  }

  return( 0 )
}



# Helper function to get the dimension names for the features that
# should be put on a plot.
get_dimnames <- function() {
  # Create a data frame with 'feature' and 'fname' columns

  df <- tibble::tribble(
                                ~feature,                ~fname,
                               "achieve",         "Achievement",
                                   "adj",          "Adjectives",
                                "affect",              "Affect",
                           "affiliation",         "Affiliation",
                              "Analytic", "Analytical thinking",
                                "assent",              "Assent",
                             "Authentic",        "Authenticity",
                                 "cause",           "Causation",
                                 "Clout",               "Clout",
                               "cogproc",     "Cognitive proc.",
                                  "conj",        "Conjunctions",
                                   "Dic",          "Dictionary",
                               "discrep",         "Discrepancy",
                                "drives",              "Drives",
                                "family",              "Family",
                                "female",              "Female",
                        "Flesch.Kincaid",         "Readability",
                             "focuspast",       "Focus on past",
                               "leisure",             "Leisure",
                                  "male",                "Male",
                                "motion",              "Motion",
                                "posemo",    "Positive emotion",
                                 "QMark",      "Question marks",
                                  "risk",                "Risk",
                                "Sixltr",   "Six+ letter terms",
                                "social",        "Social proc.",
                                "tentat",           "Tentative",
                                  "Tone",      "Emotional tone",
                                   "TTR",    "Type-token ratio",
                                    "WC",          "Word count",
                                   "WPS",  "Words per sentence",
                                   "XXX",     "Illegible terms",
                                   "xxx",     "Illegible terms",
                       "pronoun_density",     "Pronoun density",
                              "interrog",      "Interrogatives",
                    "all_demonstratives",      "Demonstratives",
                              "informal",   "Informal language",
                               "percept",          "Perception"
  )

  return( as.data.frame( df ) )
}

