#' Compute document-level feature vectors from a pre-trained embedding
#' model.
#'
#' This function generates a vector embedding for each word in a
#' string using a set of pre-trained word vectors such as GloVe
#' \insertCite{pennington2014glove}{tada} and returns the mean vector
#' projection across all words in a document.
#'
#' @import tm
#'
#' @inheritParams generate_features
#'
#' @param glove Logical.  True means use pre-trained GloVe embedding
#'   model.
#' @param dim Dimension(s) of the pre-trained GloVe embedding model;
#'   allowed values are 50 (default), 100, 200, and 300
#' @param model User-specified model object pointing to a custom
#'   pre-trained Word2Vec model, represented as a matrix with rownames
#'   of the words and rows being the embeddings for each word.
#' @return A list of data frames containing the Word2Vec projections
#'   of the corpus
#'
#' @references{ \insertRef{mikolov2013efficient}{tada}
#' \insertRef{pennington2014glove}{tada} }
#' @export
extract_w2v <- function(x,
                        meta = NULL,
                        glove=TRUE, dim=50,
                        model=NULL) {

  if ( !is.null(meta) ) {
    stopifnot( length(x) == nrow(meta) )
  }

  # Loop over list of dimensions, calling ourselves to build up full
  # list
  if ( is.null(model) && length( dim ) > 1 ) {
    proj = meta
    for ( d in dim ) {
      proj = extract_w2v( x = x, meta = proj,
                          glove = glove, dim = d, model = model )
    }
  }

  if ( is.null(model) ) {
    # Load model if not passed
    stopifnot( all(dim) %in% c( 50, 100, 200, 300 ) )

    fname=paste0("glove.",dim,"d")
    data(list=fname)
    glove = get(fname)
    stopifnot( ncol(glove) == dim )
  } else {
    glove = model
    dim = ncol(glove)
  }

  glove = rownames_to_column(glove, var = "word")

  #glove.vocab = unique(removePunctuation(names(glove.50d)))

  proj = softmaxreg::wordEmbed(essay.text, dictionary=glove, meanVec=TRUE)
  proj = as.data.frame(proj)
  names(proj) = paste0("W2V.d", 1:dim )

  if ( !is.null( meta ) ) {
    stopifnot( nrow(meta) == nrow(proj) )
    proj = cbind( meta, proj )
  }

  return(proj)

}

