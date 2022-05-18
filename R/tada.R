
#' Generate an array of text features
#'
#' Generates a rich feature representation for documents provided as a character vector or \link{quanteda::corpus()} object
#' by applying an array of linguistic and syntactic indices, available text analysis dictionaries,
#' and pre-trained embedding models to all documents.
#'
#' @import quanteda
#' @import quanteda.textstats
#' @import quanteda.sentiment
#' @import quanteda.dictionaries
#' @import tm
#' @import caret
#'
#' @param x A \link{corpus} object or character vector of text documents.
#' @param lex Logical, indicating whether to compute lexical indices including measures of lexical diversity, readability,
#' and entropy
#' @param sent Logical, indicating whether to compute sentiment analysis features from available dictionaries
#' @param ld character vector defining lexical diversity measures to compute; see \link{quanteda.textstats::textstat_lexdiv()}
#' @param read character vector defining readability measures to compute; see \link{quanteda.textstats::textstat_readability()}
#' @param terms character vector of terms to evaluate as standalone features based on document-level frequency (case-insensitive)
#' @param preProc Named list of arguments passed to \code{caret::preProcess()} for applying pre-processing transformations across the set of text features (e.g., removing collinear features)
#' @param ... (optional) additional arguments passed to \link{quanteda::tokens()} for text pre-processing.
#' @return A matrix of available text features, one row per document, one column per feature.
#' @references{
#' \insertRef{pennington2014glove}{tada}
#' }
#' @export

tada <- function(x, lex=TRUE, sent=TRUE,
                 ld="all", read=c('ARI','Coleman','DRP','ELF',
                                  'Flesch','Flesch.Kincaid','meanWordSyllables'),
                 terms = NULL,
                 preProc=list(uniqueCut=1, freqCut=99/1, cor=0.95, remove.lc=TRUE)){

  raw = x
  clean = tm::removeNumbers(clean_txt(raw))

  tok.clean = quanteda::tokens(clean)
  dfm.clean = quanteda::dfm(tok.clean)

  all.feats=data.frame()

  if (lex){
  f.ld = quanteda.textstats::textstat_lexdiv(dfm.clean, measure=ld, remove_numbers=F, remove_punct=T,
                                             remove_symbols=F)
  f.read = quanteda.textstats::textstat_readability(raw, measure=read, intermediate = F)
  f.ent = quanteda.textstats::textstat_entropy(dfm.clean, margin="documents")
  all.feats = cbind(f.ld[,-c(1)], f.read[,-c(1)], f.ent[,-c(1)])
  }

  if (sent){
  # Valence scores

  dics = list(quanteda.sentiment::data_dictionary_AFINN, quanteda.sentiment::data_dictionary_ANEW, quanteda.sentiment::data_dictionary_sentiws)
  names(dics)=c("AFINN","ANEW","sentiws")
  val = do.call(cbind, lapply(dics, function(x) textstat_valence(tok.clean, dictionary=x)[,2]))

  polarity(quanteda.sentiment::data_dictionary_LSD2015) <- list(
    pos = c("positive", "neg_negative"),
    neg = c("negative", "neg_positive")
  )
  dics2 = list(quanteda.sentiment::data_dictionary_LSD2015, quanteda.sentiment::data_dictionary_NRC, quanteda.sentiment::data_dictionary_LoughranMcDonald)
  pol = do.call(cbind, lapply(dics2, function(x) textstat_polarity(tok.clean, dictionary=x)[,2]))
  names(pol)=c("LSD2015","NRC","LoughranMcDonald")


  corp = quanteda::corpus(raw)
  sent= liwcalike(corp, dictionary = quanteda.dictionaries::data_dictionary_RID)
  sent = sent %>% dplyr::select(WPS:OtherP)

  sent1= liwcalike(corp, dictionary=quanteda.dictionaries::data_dictionary_MFD)
  sent1 = dplyr::select(sent1, care.virtue:sanctity.vice)

  sent2 = liwcalike(corp, dictionary=quanteda.sentiment::data_dictionary_LoughranMcDonald)
  sent2 = dplyr::select(sent2, negative:`modal words strong`)

  sent = cbind(sent, sent1, sent2)


  new = cbind(val, pol, sent)
  zv = apply(new,2,function(x)length(unique(x))==1)
  new = new[,!zv]

  add = new[,!names(new)%in%names(all.feats)]
  all.feats = cbind(all.feats, add)
  }

  all.feats$WCperChar = all.feats$WC/nchar(clean)



  if (preProc$remove.lc==TRUE){
    lc=caret::findLinearCombos(all.feats)
    if (!is.null(lc$remove)){
      all.feats = all.feats[,-c(lc$remove)]
    }
  }
  nz = caret::nearZeroVar(all.feats, uniqueCut=preProc$uniqueCut, freqCut=preProc$freqCut)
  out = all.feats[,-c(nz)]

  cor = caret::findCorrelation(cor(out), cutoff=preProc$cor)
  out = out[,-c(cor)]

  if (!is.null(terms)){
  dfm.freq = quanteda::dfm(quanteda::tokens(clean_txt(raw)))
  tmp=as.matrix(dfm.freq)[,which(colnames(dfm.freq)%in%terms)]
    if (length(terms) == 1){
    tmp = as.data.frame(tmp); rownames(tmp)=NULL
    names(tmp)=terms
    }
  out = as.data.frame(cbind(out, tmp))

  }

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

