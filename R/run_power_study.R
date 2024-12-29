#' Simulate power under different text scoring configurations.
#'
#' This function...
#'
#' @param n.coded Number of hand-coded documents
#' @param n.auto Number of additional documents to be scored by ML model.
#' @param d Standardized effect size
#' @param power Power of test (1 minus Type II error probability)
#' @param sig.level Significance level (Type I error probability)
#' @param n.sim Number of simulations used for power calculations
#' @return Object of class \code{power.textML}
#' @keywords internal
#'


run_power_study = function(n.coded=NULL, n.auto=NULL,
                           d=NULL,
                           power=0.8, sig.level=0.05){


  warning( "This method not yet implemented" )

}
