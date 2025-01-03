
#' Plot the results from an impact analysis with text outcomes
#'
#' This function provides a visualization of the set of textual
#' features found to differ systematically between treatment and
#' control groups.
#'
#' @importFrom plotrix plotCI
#' @import scales
#' @import dplyr
#'
#' @param out a model object output from \code{estimate_impacts()}
#' @param alpha the threshold for determining statistical significance
#' @param cols should effects be colored by direction (red for
#'   negative impacts, blue for positive impacts)
#' @param group (optional) should effects be grouped by category
#'   (e.g., higher-level summary measures, linguistic features, etc.)
#' @param xlim (optional) bounds for x-axis
#' @param ... additional arguments passed to plot
#' @export

plot_textfx= function( out, alpha=0.05, cols=FALSE, group=TRUE,
                       xlim=NULL, ...){
  out1 = out
  out1 = standardize_impact_table( out1 )

  R = nrow(out1)
  out1 <- out1 %>%
    dplyr::filter( scale > 0 )
  if ( nrow(out1) < R ) {
    warning( "Features dropped due to no variation in tx or co group" )
  }

  out1$ord = rep(3,nrow(out1))
  if (group){
    out1$ord[out1$feature %in% c("Analytic","Authentic","Clout","Tone")] = 1
    out1$ord[out1$feature %in% c("WC","WPS","TTR","xxx","Sixltr",
                                 "Flesch.Kincaid", "Flesch","XXX",
                                 "R","ARI")] = 2
  }
  dimnames = get_dimnames()
  out1 = merge(out1, dimnames, by="feature",all.x=T)
  out1$fname[is.na(out1$fname)]=out1$feature[is.na(out1$fname)]
  out1 = dplyr::arrange(out1, desc(ord), desc(fname))

  ptcols="gray"
  if (cols){ptcols= ifelse(out1$est>0,"blue","red")}
  if (!is.null(xlim)){
    xl=xlim[1]
    xr=xlim[2]
  } else {
    xl=min(out1$conf.low_std)-0.1
    xr=max(out1$conf.high_std)+0.1
  }

  xlabel = "Standardized difference in means (95% CI)"

  plotrix::plotCI(x=out1$estimate_std, y=1:nrow(out1),
                  ui=out1$conf.high_std, li=out1$conf.low_std,
                  err="x",yaxt="n",ylab="",xlab=xlabel,
                  pch=NA,scol="white",xlim=c(xl-0.05,xr+0.05),xaxt="n",...)
  axis(side=1,at=seq(xl, xr, by=0.2),labels=sprintf("%0.1f",seq(xl,xr,0.2)))
  abline(v=0,lty=2)
  if (length(unique(out1$ord))>2){
    ind=c(min(which(out1$ord==1)),min(which(out1$ord==2)))-0.5
    abline(h=ind[1],col="darkgray",lty=3,lwd=2)
    abline(h=ind[2],col="darkgray",lty=3,lwd=2)
  }


  linecols = ifelse(out1$p.adj<=alpha, 1,0.5)
  ptwt = ifelse(out1$p.adj<=alpha, 0.75, 0.1)
  plotrix::plotCI(x=out1$estimate_std, y=1:nrow(out1), ui=out1$conf.high_std, li=out1$conf.low_std,
                  err="x",add=T,scol=alpha("black",linecols), col=alpha("black",linecols),
                  pch=21, pt.bg=alpha(ptcols,ptwt), cex=1.3,lwd=1.25)

  which.sigs = out1$p.adj<=alpha
  y1 = (1:nrow(out1))[!which.sigs]
  y2 = (1:nrow(out1))[which.sigs]


  if ( length(y1) > 0 ) {
    axis(side=2,at=y1,labels=out1$fname[!which.sigs],las=2,cex.axis=0.875,tcl=-0.25)
  }
  if ( length(y2) > 0 ) {
    axis(side=2,at=y2,labels=out1$fname[which.sigs],las=2,cex.axis=0.95,tcl=-0.25,
         font=2)
  }

}




get_dimnames <- function() {
  # Create a data frame with 'feature' and 'fname' columns
  df <- data.frame(
    feature = c(
      "achieve", "adj", "affect", "affiliation", "Analytic", "assent", "Authentic",
      "cause", "Clout", "cogproc", "conj", "Dic", "discrep", "drives", "family",
      "female", "Flesch.Kincaid", "focuspast", "leisure", "male", "motion",
      "posemo", "QMark", "risk", "Sixltr", "social", "tentat", "Tone", "TTR",
      "WC", "WPS", "XXX","xxx", "pronoun_density", "interrog", "all_demonstratives", "informal","percept"
    ),
    fname = c(
      "Achievement", "Adjectives", "Affect", "Affiliation", "Analytical thinking",
      "Assent", "Authenticity", "Causation", "Clout", "Cognitive proc.", "Conjunctions",
      "Dictionary", "Discrepancy", "Drives", "Family", "Female", "Readability",
      "Focus on past", "Leisure", "Male", "Motion", "Positive emotion", "Question marks",
      "Risk", "Six+ letter terms", "Social proc.", "Tentative", "Emotional tone",
      "Type-token ratio", "Word count", "Words per sentence", "Illegible terms","Illegible terms",
      "Pronoun density", "Interrogatives", "Demonstratives", "Informal language", "Perception"
    )
  )
  return(df)
}

