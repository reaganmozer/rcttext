

# Unused at moment
# #' @param dim Dimension(s) of the pre-trained GloVe embedding model;
# #'   allowed values are 50 (default), 100, 200, and 300
# #' @param glove Logical.  True means use pre-trained GloVe embedding
# #'   model.



#' Compute document-level feature vectors from a pre-trained embedding
#' model.
#'
#' This function generates a vector embedding for each word in a
#' string using a set of pre-trained word vectors such as GloVe
#' \insertCite{pennington2014glove}{rcttext} and returns the mean vector
#' projection across all words in a document.
#'
#' @import quanteda
#' @import dplyr
#'
#' @inheritParams generate_features
#'

#' @param model User-specified model object pointing to a custom
#'   pre-trained embedding model, represented as a matrix or data frame where the
#'   first column is the word/token and the following columns are numeric vectors.  If
#'   NULL, use default "mini_glove" embeddings on 1000 common words
#'   (not recommended).
#' @return A list of data frames containing the Word2Vec projections
#'   of the corpus
#'
#' @references{ \insertRef{mikolov2013efficient}{rcttext}
#' \insertRef{pennington2014glove}{rcttext} }
#' @export
extract_w2v <- function(x,
                        meta = NULL,
                        #   glove=TRUE, #dim=50,
                        model=NULL) {

  if ( !is.null(meta) ) {
    stopifnot( length(x) == nrow(meta) )
  }

  # Loop over list of dimensions, calling ourselves to build up full
  # list
  # if ( is.null(model) && length( dim ) > 1 ) {
  #   proj = meta
  #   for ( d in dim ) {
  #     proj = extract_w2v( x = x, meta = proj,
  #                         glove = glove, dim = d, model = model )
  #   }
  # }

  if ( is.null(model) ) {
    # Load model if not passed
    # stopifnot( all(dim) %in% c( 50, 100, 200, 300 ) )
    #
    # fname=paste0("glove.",dim,"d")
    # data(list=fname)
    # glove = get(fname)
    # stopifnot( ncol(glove) == dim )
    data( "mini_glove" )
    glove = mini_glove
  } else {
    glove = model
  }

  # Subset to only those terms used in the target text
  glove.vocab = sort(unique(glove$token))

  dfm = quanteda::dfm(quanteda::tokens(tolower(x)))
  txt.vocab=sort(unique(colnames(dfm)))

  terms.keep = intersect(glove.vocab, txt.vocab)

  glove.sub = glove %>% filter(token %in% terms.keep) %>% arrange(token)
  dfm.sub = convert(quanteda::dfm_keep(dfm, terms.keep), to="data.frame") %>% select(all_of(terms.keep))

  stopifnot(all.equal(glove.sub$token, names(dfm.sub))) # check that all the tokens are in the same order

  proj = as.matrix(dfm.sub) %*% (glove.sub %>% select(-token) %>% as.matrix())
  proj = as.data.frame(proj / rowSums(dfm.sub))


  if ( !is.null( meta ) ) {
    stopifnot( nrow(meta) == nrow(proj) )
    proj = cbind( meta, proj )
  }

  return(proj)

}

