



#' Generic text cleaning function
#'
#' Quick and easy text cleaning.  Take given copus and
#' remove punctuation, remove whitespace, convert everything
#' to lowercase.
#'
#' @param x Character vector or corpus object.
#'
#' @export
clean_text = function(x){
  require(tm)
  tmp = gsub(".", " . ", x, fixed=TRUE)
  tmp = gsub(",", " . ", tmp, fixed=TRUE)
  tmp = gsub("-", "  ", tmp, fixed=TRUE)
  tmp = stripWhitespace(removePunctuation(tolower(tmp)))
  return(tmp)
}
