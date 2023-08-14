



#' Generic text cleaning function
#'
#' Quick and easy text cleaning.  Take given copus and
#' remove punctuation, remove whitespace, convert everything
#' to lowercase. This function is used for pre-processing text
#' within the generate_features function. The main use for
#' clean_text is to check which elements of your text
#' are converted to empty strings. Empty strings may result in clean_features
#' dropping desired columns.
#'
#' @import tm
#'
#' @param x Character vector or corpus object.
#' @param split_hyphens A logical indicating whether hypenated words should
#' be treated as two tokens (split at the hypen).
#'
#' @export
clean_text = function(x, split_hyphens=TRUE){
  require(tm)
  if (split_hyphens) {
    x <- gsub('-', ' ', x)
  }
  tmp <- x %>% tolower() %>%
    tm::removePunctuation() %>%
    tm::stripWhitespace() %>%
    tm::removeNumbers()
  return(tmp)
}
