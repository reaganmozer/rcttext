#' Compute document-level feature vectors from a pre-trained embedding model.
#'
#' This function generates a vector embedding for each word in a string using a
#' set of pre-trained word vectors such as GloVe
#' \insertCite{pennington2014glove}{tada} and returns the mean vector projection
#' across all words in a document.
#'
#' @import tm
#'
#'
#' @param x A \link{corpus()} object or character vector of text documents.
#' @param glove Logical.  True means use pre-trained GloVe embedding model.
#' @param dim Dimension(s) of the pre-trained GloVe embedding model; allowed values are 50 (default), 100, 200, and 300
#' @param model User-specified model object pointing to a custom pre-trained
#'   Word2Vec model.
#' @return A list of data frames containing the Word2Vec projections of the
#'   corpus
#' @references{ \insertRef{mikolov2013efficient}{tada}
#' \insertRef{pennington2014glove}{tada} }
#' @export
#'
extract_w2v <- function(x, glove=TRUE, dim=50,
                       model=NULL){

  for (j in 1:length(dim)){
    fname=paste0("glove.",dim,"d")
    data(list=fname)
    glove = get(fname)
  }
  glove.vocab = unique(tm::removePunctuation(names(glove)))
  glove = data.frame(t(glove))
  glove$words = rownames(glove)
  k = ncol(glove)
  glove2 = glove[,c(k, 1:(k-1))]

  proj = softmaxreg::wordEmbed(x, dictionary=glove2, meanVec=TRUE)
  proj = as.data.frame(proj)
  names(proj) = paste0("W2V.d", 1:50)
  return(proj)

}

