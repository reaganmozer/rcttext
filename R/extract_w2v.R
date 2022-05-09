#' Compute document-level feature vectors from a pre-trained embedding model.
#'
#' This function generates a vector embedding for each word in a string using a
#' set of pre-trained word vectors such as GloVe
#' \insertCite{pennington2014glove}{tada} and returns the mean vector projection
#' across all words in a document.
#'
#'
#' @param x A \link{corpus()} object or character vector of text documents.
#' @param glove Logical.  True means use pre-trained GloVe embedding model.
#' @param dim Dimension(s) of the pre-trained GloVe embedding model. Ignored if
#'   \code{glove=FALSE}.
#' @param model User-specified model object pointing to a custom pre-trained
#'   Word2Vec model.
#' @return A list of data frames containing the Word2Vec projections of the
#'   corpus
#' @references{ \insertRef{mikolov2013efficient}{tada}
#' \insertRef{pennington2014glove}{tada} }
#'
#'@keywords internal
extract_w2v <- function(x, glove=TRUE, dim=c(50,100,200,300),
                       model=NULL){


}
