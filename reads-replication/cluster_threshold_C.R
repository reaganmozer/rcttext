


#' Determine the penalty C that will zero out the textreg model for a series of
#' randomly permuted labelings with random assignment dictated by a blocked and
#' cluster-randomized experiment.
#'
#' Method repeatidly generates +1/-1 vectors within the given blocking structure
#' with blocks of +1/-1 within the clustering vector, and then finds a threshold
#' C for each permutation.
#'
#' @return List of numbers.  First is the threshold C for the passed labeling.
#'   Remainder are the reference distribution based on the permutations.
cluster.threshold.C = function (corpus, labeling, cluster_id, block_id = NULL, R, ...) {
    
    if ( !is.null( block_id ) ) {
        stopifnot( length( labeling ) == length( block_id ) )
    }
    stopifnot( length( labeling ) == length( cluster_id ) )
    
    if (is.textreg.corpus(corpus)) {
        if (!missing(labeling)) {
            warning("New labeling not used with pre-built corpus at this time")
        }
    }
    
    Rorig = find.threshold.C(corpus, labeling, R=0, ... )
    
    if ( R > 0 ) {
        Rs = map_dbl( 1:R, function( r ) {
            aa = NULL
            if ( is.null( block_id ) ) {
                aa = randomizr::cluster_ra( clusters=cluster_id )
            } else {
                aa = randomizr::block_and_cluster_ra( blocks = block_id, clusters=cluster_id )
            }
            find.threshold.C( corpus, 2*aa-1, R = 0, ... )
        })
        Rorig = c( Rorig, Rs )
    }
    
    as.numeric( Rorig )
}
