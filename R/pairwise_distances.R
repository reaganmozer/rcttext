

#' Calculate all pairwise distances between documents in two corpus.
#'
#' @param wide If TRUE return distances with each column corresponding
#'   to a document in corpus0, and each row a document in corpus1.  If
#'   FALSE return dataframe of indices for each pair, with the third
#'   column being distance.
#' @param model When using w2v, a user-specified model object pointing
#'   to a pre-trained embedding model, represented as a data frame
#'   where the first column is the word/token and the following
#'   columns are numeric vectors.
#' @return Dataframe of either length(corpus1) rows or
#'   length(corpus1)*length(corpus0) rows, depending on wide.
#' @export
pairwise_distances <- function( corpus1, corpus0,
                                method = c( "cosine", "w2v" ),
                                wide = TRUE,
                                model = NULL,
                                use_names = TRUE,
                                names_prefix = "doc_" ) {

  method = match.arg(method)

  n0 = length( corpus0 )
  n1 = length( corpus1 )

  Z = c(rep(0,n0), rep(1,n1)) # Indicator for reference texts

  all.raw = c( corpus0, corpus1 )
  names( all.raw ) <- NULL

  if ( method == "cosine" ) {

    # Calculate cosine distance to each reference text
    dfm1 = quanteda::dfm(quanteda::tokens(all.raw))
    tdm.dists = textmatch::pair_distances(dfm1, Z, include="cosine", form="data.frame")
  } else {

    # Calculate distance in w2v projections to each reference text
    proj.all = extract_w2v( clean_text(all.raw),
                            model = model )

    tdm.dists = textmatch::pair_distances(proj.all, Z, include="cosine",form="data.frame")
  }

  # NOTE: textmatch seems to flip treatment and control units
  if ( min( tdm.dists$index.0 ) != 1 ) {
    colnames(tdm.dists)[c(1, 2)] <- colnames(tdm.dists)[c(2, 1)]
  }
  tdm.dists$index.1 = tdm.dists$index.1 - n0

  if ( use_names ) {
    if ( !is.null( names( corpus0 ) ) ) {
      tdm.dists$index.0 = names( corpus0 )[tdm.dists$index.0]
    }
    if ( !is.null( names( corpus1 ) ) ) {
      tdm.dists$index.1 = names( corpus1 )[tdm.dists$index.1]
    }
  }

  if ( wide ) {

    if ( is.null( names( corpus0 ) ) ) {
      # Determine the maximum number of digits
      max_digits <- nchar(as.character(max(tdm.dists$index.0)))
      tdm.dists$index.0 <- sprintf(paste0("%0", max_digits, "d"), tdm.dists$index.0)
    }

    all.dists <- tdm.dists %>%
      pivot_wider(names_from = index.0, values_from = cosine.dist, names_prefix = names_prefix)

    stopifnot( ncol(all.dists) == n0 + 1)

  } else {
    all.dists = tdm.dists[with(tdm.dists,order(index.0, as.numeric(index.1))),]
  }

  all.dists
}



#' Generate distances to a set of reference documents
#'
#' This is a helper function for the pairwise_distances method.
#'
#' @inheritParams generate_features
#' @inheritParams pairwise_distances
#'
#' @param reference_docs A list of reference documents to compare
#'   corpus against.  Will end up with one distance feature per
#'   reference doc.
#' @seealso \code{\link{pairwise_distances}}
#' @export
#'
generate_distance_features <- function(x,
                              meta = NULL,
                              reference_docs,
                              verbose = FALSE,
                              method = c( "cosine", "w2v" ),
                              model = NULL,
                              use_names = TRUE,
                              names_prefix = "doc_" ) {

  stopifnot( !is.null( reference_docs ) )
  stopifnot( is.character( reference_docs ) )

  all.dists = pairwise_distances(x, reference_docs,
                                 method = method, wide = TRUE,
                                 model = model, use_names = use_names,
                                 names_prefix = names_prefix )

  if ( any( is.na( all.dists ) ) ) {
    warning( "NA or NaN values found in similarity matrix." )
  }

  res <- bind_cols( meta, all.dists )
  res$index.1 = NULL


  res
}

