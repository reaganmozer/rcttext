
#' Generate an array of text features
#'
#' Generates a rich feature representation for documents provided as a character vector or \link{quanteda::corpus()} object
#' by applying an array of linguistic and syntactic indices, available text analysis dictionaries,
#' and pre-trained embedding models to all documents.
#'
#' @import quanteda
#' @import quanteda.textstats
#' @import quanteda.dictionaries
#' @import quanteda.sentiment
#' @import tm
#'
#' @param text A \link{corpus} object or character vector of text documents.
#' @param lex Logical, indicating whether to compute lexical indices including measures of lexical diversity, readability,
#' and entropy
#' @param sent Logical, indicating whether to compute sentiment analysis features from available dictionaries
#' @param embed Number of word embedding dimensions to compute for each document using GloVe pre-trained embedding models \insertCite{pennington2014glove}{tada}.
#' Defaults to 300.
#' @param p_max The maximum number of features to compute. Defaults to \code{NULL} (no strict limit).
#' @param preProc Named list of arguments passed to \code{caret::preProcess()} for applying pre-processing transformations across the set of text features (e.g., removing collinear features)
#' @param ... (optional) additional arguments passed to \link{tokens()} for text pre-processing.
#' @return A matrix of available text features, one row per document, one column per feature.
#' @references{
#' \insertRef{pennington2014glove}{tada}
#' }
#' @export

tada <- function(text, lex=TRUE, sent=TRUE,
                             glove=c(300,200, 100, 50), p_max=NULL,
                             preProc=list(uniqueCut=1, cor=0.9, remove.lc=TRUE)){

  corp=corpus(text)
  tok=tokens(text)
  dfm = dfm(removeNumbers(clean_txt(text)))
  ld = textstat_lexdiv(dfm, measure=c("all"))
  read = textstat_readability(text$text.sc, measure=c('ARI','Coleman','DRP','ELF',
                                                      'Flesch','Flesch.Kincaid','meanWordSyllables'), intermediate = T)
  ent = textstat_entropy(dfm(text$text.sc), margin="documents")
  all.feats = cbind(ld[,-c(1)], read[,-c(1)], ent[,-c(1)])




  # Valence scores
  dics = list(data_dictionary_AFINN, data_dictionary_ANEW, data_dictionary_sentiws)
  names(dics)=c("AFINN","ANEW","sentiws")
  val = do.call(cbind, lapply(dics, function(x) textstat_valence(tok, dictionary=x)[,2]))

  polarity(data_dictionary_LSD2015) <- list(
    pos = c("positive", "neg_negative"),
    neg = c("negative", "neg_positive")
  )
  dics2 = list(data_dictionary_LSD2015, data_dictionary_NRC, data_dictionary_LoughranMcDonald)
  pol = do.call(cbind, lapply(dics2, function(x) textstat_polarity(tok, dictionary=x)[,2]))
  names(pol)=c("LSD2015","NRC","LoughranMcDonald")


  sent= liwcalike(corp, dictionary = data_dictionary_RID)
  sent = sent %>% select(starts_with("secondary."),starts_with("emotions."))

  sent1= liwcalike(corp, dictionary=data_dictionary_MFD)
  sent1 = select(sent1, care.virtue:sanctity.vice)

  sent2 = liwcalike(corp, dictionary=data_dictionary_LoughranMcDonald)
  sent2 = select(sent2, negative:`modal words strong`)

  sent = cbind(sent, sent1, sent2)


  new = cbind(val, pol, sent)
  zv = apply(new,2,function(x)length(unique(x))==1)
  new = new[,!zv]

  add = new[,!names(new)%in%names(all.feats)]
  all.feats = cbind(all.feats, add)
  out = preProcess(all.feats, method=c("corr","nzv"), uniqueCut=uniqueCut, cutoff=cor)
  return(out)


}

clean_txt = function(x){
  require(tm)
  tmp = gsub(".", " . ", x, fixed=TRUE)
  tmp = gsub(",", " . ", tmp, fixed=TRUE)
  tmp = gsub("-", "  ", tmp, fixed=TRUE)
  tmp = stripWhitespace(removePunctuation(tolower(tmp)))
  return(tmp)
}

