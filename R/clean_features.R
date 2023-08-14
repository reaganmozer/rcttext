

#' Clean and simplify feature set
#'
#' Given set of generated features, simplify set of features using
#' carat package.
#'
#' @param meta The set of features
#' @param ignore List of column names to ignore when simplifying
#'   (e.g., ID column and other columns that should be preserved).
#' @param uniqueCut  Param for carat's nearZeroVar
#' @param freqCut Param for carat's nearZeroVar
#' @param cor Cutoff of how correlated features should be before dropping one.
#' @param remove.lc TRUE means remove colinear combinations of features.
#' @param verbose Print out progress to console.
#' @return Updated meta with fewer columns of the preserved features.
clean_features <- function( meta, ignore = NULL,
                            remove.lc = TRUE,
                            uniqueCut = 1, freqCut = 99,
                            cor = 0.95,
                            verbose = FALSE ) {

  require( caret )
  is.mat = is.matrix(meta)
  meta_aside = NULL

  if ( !is.null(ignore) ) {
    set_aside = match(ignore, colnames(meta))
    if ( any( is.na( set_aside ) ) ) {
      warning( paste0( "Asked to ignore columns that were not found: '",
                       paste0(ignore[is.na(set_aside)], sep="," ),
                       "'." ) )
      set_aside = set_aside[!is.na(set_aside)]
    }
    meta_aside = meta[,set_aside]
    meta = as.matrix( meta[,-set_aside] )
  } else {
    meta = as.matrix( meta )
  }

  stopifnot( is.numeric(meta) )
  baddies = is.infinite(meta) | is.nan(meta) | is.na(meta)
  cols = apply(baddies, 2, sum )
  if ( !(all(cols == 0) ) ) {
    warning( sprintf( "Found infinite/nan/na values in feature set: %s.  Percent bad: %.3f. Dropping those features.",
                     paste0( which(cols!=0), collapse=", " ),
                     mean(baddies) ) )
    meta = meta[, cols == 0]
  }

  if (remove.lc==TRUE){
    vcat( verbose, "Removing colinear combinations of features" )
    lc=caret::findLinearCombos(meta)
    if (!is.null(lc$remove)){
      meta = meta[,-c(lc$remove)]
    }
  }

  vcat( verbose, "Dropping low-variaton features" )
  nz = caret::nearZeroVar(meta, uniqueCut=uniqueCut, freqCut=freqCut)
  if ( length( nz ) > 0 ) {
    meta = meta[,-c(nz)]
  }

  vcat( verbose, "Dropping features that are too correlated" )
  cor = caret::findCorrelation(cor(meta), cutoff=cor)
  if ( length(cor) > 0 ) {
    meta = meta[,-c(cor)]
  }

  if ( !is.null(meta_aside) ) {
    cbind( meta_aside, meta )
  } else {
    meta
  }
}
