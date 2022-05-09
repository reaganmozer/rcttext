#' Perform Concise Comparative Summarization across a grid of tuning parameters
#'
#' Wrapper for \link{textreg::textreg()}.
#'
#' @param x a corpus, character vector of text documents, or set of text features.
#' @param Z an indicator for treatment assignment
#' @param clusterID optional vector of cluster ID's
#' @param ... additional arguments passed to \link{textreg()}.
#' @return a \link{textreg.result()} object.
#' @export

run_ccs = function( x, Z, clusterID=NULL) {


  C = cluster.threshold.C( sc, more_sc, cluster_id=cluster_id, R = 10)
  C
  quantile(C,0.80)
  scat( "L2 C: %.2f / %.2f\n", C[[1]], quantile(C,0.80) )
  C_L2 = ceiling( quantile(C,0.80) )
  res1 = textreg( corpus = sc,
                  more_sc,
                  C = C_L2,
                  verbosity = 0)
  res1

  res2 = textreg( corpus = sc,
                  more_sc,
                  C = C_L2, gap=1,
                  verbosity = 0)
  res2

  res3 = textreg( corpus = sc,
                  more_sc,
                  C = C_L2, binary.features = TRUE,
                  verbosity = 0)
  res3


  C = cluster.threshold.C( sc, more_sc, Lq = 3, cluster=cluster_id, R=10 )
  C
  C_L3 = ceiling( median( C ) )
  quantile(C,0.80)
  scat( "L3 C: %.2f / %.2f\n", C[[1]], quantile(C,0.80) )

  res4 = textreg( corpus = sc,
                  more_sc,
                  C = C_L3,
                  Lq = 3,
                  verbosity = 0)
  res4



  C = cluster.threshold.C( sc, more_sc, Lq = 1.5, cluster=cluster_id, R=10 )
  C
  quantile(C,0.80)
  C_L1.5 = ceiling( quantile(C,0.80) )
  scat( "L1.5 C: %.2f / %.2f\n", C[[1]], quantile(C,0.80) )

  res5 = textreg( corpus = sc,
                  more_sc,
                  C = C_L1.5,
                  Lq = 1.5,
                  verbosity = 0)
  res5

  results = list(  res1, res2,  res3, res4, res5 )
  for ( i in 1:length(results) ) {
    results[[i]]$model$ngram = gsub( " \\*$", "", results[[i]]$model$ngram )
  }

  tbl = make.list.table( results,
                         model.names = paste( c("L2","L2 (gap)","L2 (bin)","L3","L1.5"),
                                              round( c(C_L2, C_L2,C_L2,C_L3,C_L1.5),digits=1),
                                              sep="-" ),
                         method = "rank" )

  tbl
}

#' Make results table for grid CCS run
#'
#' @import dplyr
#'
#' @param result a \link{textreg.result()} object
#' @param corp a corpus or character vector to calculate term frequencies across
#' @param Z an indicator for treatment assignment
#' @param clusterID optional vector of cluster ID's
#' @param ... additional arguments passed to \link{textreg()}.
#' @return a \link{textreg.result()} object.

results.tab = function(result, corp, Z){
  result$n.mods = sapply(1:nrow(result),function(x) sum(!is.na(result[x,c(2:6)])))
  tmp = subset(result,select=c(phrase, n.mods, num.reports, num.tag))
  tmp$count.neg = tmp$num.reports-tmp$num.tag
  names(tmp)=c("phrase","n.mods","docs.total", "docs1","docs0")
  tmp2 = tmp %>% mutate(prop.docs1= docs1/sum(Z),
                        prop.docs0 = docs0/sum(1-Z),
                        prop.diff = prop.docs1-prop.docs0)


  phrases = unique(tmp2$phrase)
  m = textreg::make.phrase.matrix(phrases, corp.sub)
  tmp2$phrase.tot = colSums(m)
  tmp2$tot1 = colSums(m[Z==1,])
  tmp2$tot0 = colSums(m[Z==0,])

  out = dplyr::select(tmp2, phrase, n.mods, tot1, tot0,prop.diff)
  out$docs1 = paste0(tmp2$docs1, " (",round(tmp2$prop.docs1,2)*100, "%)")
  out$docs0 = paste0(tmp2$docs0, " (",round(tmp2$prop.docs0,2)*100, "%)")
  out$diff.val=tmp2$prop.diff
  out$diff = paste0(round(tmp2$prop.diff,2)*100,"%")
  out$diff[tmp2$prop.diff>0]=paste0("+",out$diff[tmp2$prop.diff>0])
  out = out[with(out,order(desc(n.mods),desc(prop.diff))),]
  out = dplyr::select(out, phrase, n.mods, tot1, tot0,docs1, docs0, diff, diff.val)
  rownames(out)=NULL
  out
}


#' Cluster-randomized permutation test for tuning a CCS model
#'
#' @description
#' Determine the penalty C that will zero out the textreg model for a series of
#' randomly permuted labelings with random assignment dictated by a blocked and
#' cluster-randomized experiment.
#'
#' @details
#' Method repeatedly generates +1/-1 vectors within the given blocking structure
#' with blocks of +1/-1 within the clustering vector, and then finds a threshold
#' C for each permutation.
#'
#' @import textreg
#'
#' @rdname run_ccs
#' @param design Type of design used for random assignment (complete
#'   randomization, multisite randomized, cluster randomized, and blocked and cluster randomized).
#' @param clusterID vector of cluster ID's
#' @param siteID vector of block ID's
#' @param R Number of times to scramble treatment assignment labels
#' @return List of numbers.  First is the threshold C for the passed labeling.
#'   Remainder are the reference distribution based on the permutations.

cluster.threshold.C = function (x, z,
                                design=c("crd","multi","cluster","rcbd"),
                                clusterID=NULL, siteID = NULL, R, ...) {

  if ( !is.null( block_id ) ) {
    stopifnot( length( labeling ) == length( block_id ) )
  }
  if (!is.null(clusterID)){
    stopifnot( length( labeling ) == length( clusterID ) )}


  if (is.textreg.corpus(corpus)) {
    if (!missing(labeling)) {
      warning("New labeling not used with pre-built corpus at this time")
    }
  }

  Rorig = textreg::find.threshold.C(corpus, labeling, R=0, ... )

  if ( R > 0  & !is.null(clusterID)) {
    Rs = purrr::map_dbl( 1:R, function( r ) {
      aa = NULL
      if ( is.null( block_id ) ) {
        aa = randomizr::cluster_ra( clusters=cluster_id )
      } else {
        aa = randomizr::block_and_cluster_ra( blocks = block_id, clusters=cluster_id )
      }
      textreg::find.threshold.C( corpus, 2*aa-1, R = 0, ... )
    })
    Rorig = c( Rorig, Rs )
  }

  as.numeric( Rorig )
}
