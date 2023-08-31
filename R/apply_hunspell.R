
#' Force spell-correct
#'
#' Try to spell correct unrecognized words as best as able using the
#' hunspell package.  This will take the first suggestion of hunspell
#' and just replace the text with the suggestion, word by word.
#'
#' All words of 2 or 1 character are skipped.
#'
#' @param text  The text to spell check (list of character vectors)
#' @param additional_words List of words that should be considered as
#'   spelled correctly.
#' @param skip_prefix List of prefixes that if a word has we should
#'   skip over
#' @param threshold Don't try to correct anything that only occurs
#'   threshold or less times in corpus.
#' @param to_lower If TRUE will keep everything lowercase, regardless
#'   of spelling suggestions.
#' @return Vector of text, spell-corrected we hope.
#' @export
apply_hunspell <- function( text,
                            additional_words = NULL,
                            skip_prefix = NULL,
                            threshold = 1,
                            to_lower = FALSE,
                            verbose = FALSE ) {

  library(quanteda)
  library(hunspell)
  library(tm)
  require( purrr )
  require( tidyverse )

  hunspell::dictionary("en_US",
                       add_words = additional_words)

  dfm = text %>%
    tokens() %>%
    dfm()

  vocab = colnames( dfm )

  # How many words not recognized?
  hc <- hunspell::hunspell_check(vocab,
                       hunspell::dictionary("en_US", add_words = additional_words ) )

  mis = sort(vocab[hc==FALSE])
  rm = tm::removePunctuation(mis)==""| tm::removeNumbers(mis)==""| tm::removePunctuation(tm::removeNumbers(mis))==""

  if ( !is.null(skip_prefix) ) {
    for ( prf in skip_prefix ) {
      rm = rm | startsWith(mis, prf )
    }
  }
  table(rm)
  mis = mis[!rm]
  mis = mis[nchar(mis)>3]

  # Now look at text, count number of types of mispelled words
  csums <- tibble( word = colnames(dfm),
                   count = colSums(dfm) )

  subs = tibble( word = mis )
  subs = left_join( subs, csums, by="word" )

  subs = filter( subs, count > threshold )
  subs
  if (nrow(subs) == 0 ) {
    return( text )
  }

  spellcorrect = function(x){
    hunspell::hunspell_suggest(x)[[1]][1]
  }
  subs$h = map_chr(subs$word, spellcorrect)

  subs=dplyr::filter(subs, !is.na(h) )

  if ( to_lower ) {
    subs$h = tolower(subs$h)
    subs = filter( subs, word != h )
  }
  if ( verbose ) {
    print(subs)
  }

  if ( nrow(subs) > 0 ) {
    text = repair_spelling(text, subs$word, subs$h)
  }

  text
}
