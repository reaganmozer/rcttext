
#' Generic text cleaning function
#'
#' Quick and easy text cleaning.  Take given copus and
#' remove punctuation, remove whitespace, convert everything
#' to lowercase. This function is used for pre-processing text
#' within the generate_features function.
#'
#' Strings can be collapsed to an empy string.  E.g., " . " will change to "".
#'
#' @import tm
#'
#' @param x Character vector or corpus object.
#' @param split_hyphens A logical indicating whether hyphenated words should
#'   be treated as two tokens (split at the hyphen).
#'
#' @examples
#'
#' ## Typo correction: given corpus x -> given corpus
#'
#' # Texts to be cleaned
#' txts = c( "THIS FUNCTION CONVERTS EVERYTHING TO LOWERCASE.",
#'           "This function removes punctuation........",
#'           "This function removes     whitespace.",
#'           "This-function-splits-hyphens",
#'           "The main use for clean_text is to check which elements of
#'            your text are converted to empty strings.",
#'           " " )
#'
#' ## Example 1: Convert to Lowercase
#' txts[1]
#' clean_text( txts[1] )
#'
#' ## Example 2: Remove Punctuation
#' txts[2]
#' clean_text( txts[2] )
#'
#' ## Example 3: Remove White spaces
#' txts[3]
#' clean_text( txts[3] )
#'
#' ## Example 4: Split Hyphens
#' txts[4]
#' clean_text( txts[4] )
#' clean_text( txts[4], split_hyphens = FALSE )
#'
#' ## Example 5: Clean Entire Vector of Texts)
#' clean_text( txts )
#'
#' @export

clean_text = function(x, split_hyphens=TRUE){
  #require(tm)
  if (split_hyphens) {
    x <- gsub('-', ' ', x)
  }
  tmp <- x %>% tolower() %>%
    tm::removePunctuation() %>%
    tm::stripWhitespace() %>%
    tm::removeNumbers() %>%
    stringr::str_trim()

  return(tmp)
}
