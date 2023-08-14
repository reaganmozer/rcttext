

#' Replace all words in dictionary with alternates
#'
#' Given text as a list of character strings, and a dictionary as a
#' two-column dataframe with the first column being misspelled words
#' and the second being corect spelling, swap all misspelled words
#' with the correct spellings.
#'
#' @param text Character vector
#' @param dictionary Dataframe with two columns of text, or a list of
#'   words.
#' @param to_words If dictionary is a list of words, this is list of
#'   corresponding words.
#' @param Character vector, revised version of text.
#' @export
repair_spelling <- function( text, dictionary, to_words = NULL ) {

  if ( !is.null( to_words ) ) {
    stopifnot( is.character(dictionary) )
    stopifnot( length(dictionary) == length(to_words) )
  } else {
    to_words = dictionary[[2]]
    dictionary = dictionary[[1]]
  }

  # rep_word <- function( fwrd, twrd, essay.text ) {
  #   gsub(" shoud ", " should ", essay.text, fixed=TRUE )
  # }

  dictionary = paste0( " ", dictionary, " " )
  to_words = paste0( " ", to_words, " " )

  for ( i in seq_along(dictionary) ) {
    text = gsub( dictionary[i], to_words[i], text, fixed=TRUE )
  }

  return( text )
}
