
#' Generate an array of text features
#'
#' Generates a rich feature representation for documents provided as a
#' character vector or \link{quanteda::corpus()} object by applying an
#' array of linguistic and syntactic indices, available text analysis
#' dictionaries, and pre-trained embedding models to all documents.
#'
#' @import quanteda
#' @import quanteda.textstats
#' @import quanteda.dictionaries
#' @import tm
#' @import caret
#'
#' @param x A \link{corpus} object or character vector of text
#'   documents.
#' @param meta Dataframe corresponding to the corpus x.  If passed,
#'   and non-NULL, all features will be added to this dataframe.
#' @param lex Logical, indicating whether to compute lexical indices
#'   including measures of lexical diversity, readability, and entropy
#' @param sent Logical, indicating whether to compute sentiment
#'   analysis features from available dictionaries
#' @param clean_features TRUE means implement cleaning step where we
#'   drop features with no variation and colinear features. (This
#'   happens before any term generation features are added.)
#' @param ld character vector defining lexical diversity measures to
#'   compute; see \link{quanteda.textstats::textstat_lexdiv()}
#' @param read character vector defining readability measures to
#'   compute; see \link{quanteda.textstats::textstat_readability()}
#' @param terms character vector of terms to evaluate as standalone
#'   features based on document-level frequency (case-insensitive).
#'   Not cleaned by clean_features.
#' @param preProc Named list of arguments passed to
#'   \code{caret::preProcess()} for applying pre-processing
#'   transformations across the set of text features (e.g., removing
#'   collinear features).
#' @param ... (optional) additional arguments passed to
#'   \link{quanteda::tokens()} for text pre-processing.
#' @param ignore List of column names to ignore when simplifying
#'   (e.g., ID column and other columns that should be preserved).
#' @return A data.frame of available text features, one row per document,
#'   one column per feature.
#' @export

generate_features <- function(x,
                              meta = NULL,
                              lex=TRUE, sent=TRUE,
                              ld="all",
                              clean_features = TRUE,
                              read=c( 'ARI','Coleman','DRP','ELF',
                                      'Flesch','Flesch.Kincaid','meanWordSyllables'),
                              terms = NULL,
                              preProc=list(uniqueCut=1, freqCut=99, cor=0.95, remove.lc=TRUE),
                              verbose = FALSE,
                              ignore = NULL){

  ignore = "s_id"
  if ( !is.null(meta) ) {
    stopifnot( is.data.frame(meta) )
    ignore = colnames(meta)
  }

  # utility to add on features
  add_features <- function( feats, new_feats ) {
    if ( is.null( feats ) ) {
      new_feats
    } else {
      cbind( feats, new_feats )
    }
  }

  vcat( verbose, "Generate multiple classes of features" )

  raw = x

  vcat( verbose, "Initial tokenizing and cleaning of text" )
  clean = clean_text(raw)

  # Check pre-processed texts for empty strings
  has_empties <- any(raw=='')
  if (has_empties) warning('After pre-processing, texts contain empty strings. This may result in clean_features dropping some features unexpectedly. To avoid this, remove empty texts before running this function. (Note that raw texts may not appear empty; pre-process your texts with clean_text()).')

  tok.clean = quanteda::tokens(clean)
  dfm.clean = quanteda::dfm(tok.clean)

  if ( is.null( meta ) ) {
    all.feats=NULL
  } else {
    stopifnot( is.data.frame(meta) )
    stopifnot( nrow( meta ) == nrow(dfm.clean) )
    all.feats = meta
  }

  if (lex) {
    vcat( verbose, "Calculating lexical indices" )
    f.ld = quanteda.textstats::textstat_lexdiv(dfm.clean, measure=ld,
                                               remove_numbers=F, remove_punct=T,
                                               remove_symbols=F)
    f.read = quanteda.textstats::textstat_readability(raw, measure=read, intermediate = F)
    f.ent = quanteda.textstats::textstat_entropy(dfm.clean, margin="documents")
    lex.feats = cbind(f.ld[,-c(1)], f.read[,-c(1)], f.ent[,-c(1)])
    names(lex.feats) = paste0( "lex_", names(lex.feats) )

    all.feats = add_features( all.feats, lex.feats )
  }

  if (sent){
    vcat( verbose, "Calculating sentiment scores" )

    require(quanteda.sentiment)

    # Valence scores
    dics = list(data_dictionary_AFINN, data_dictionary_ANEW, data_dictionary_sentiws)
    names(dics)=c("AFINN","ANEW","sentiws")
    val = do.call(cbind, lapply(dics, function(x) textstat_valence(tok.clean, dictionary=x)[,2]))

    polarity(data_dictionary_LSD2015) <- list(
      pos = c("positive", "neg_negative"),
      neg = c("negative", "neg_positive")
    )
    dics2 = list(data_dictionary_LSD2015, data_dictionary_NRC, data_dictionary_LoughranMcDonald)
    pol = do.call(cbind, lapply(dics2, function(x) textstat_polarity(tok.clean, dictionary=x)[,2]))
    names(pol)=c("LSD2015","NRC","LoughranMcDonald")

    corp = quanteda::corpus(raw)
    sent= liwcalike(corp, dictionary = quanteda.dictionaries::data_dictionary_RID)
    sent = sent %>% dplyr::select(WPS:OtherP)

    sent1= liwcalike(corp, dictionary=quanteda.dictionaries::data_dictionary_MFD)
    sent1 = dplyr::select(sent1, care.virtue:sanctity.vice)

    sent2 = liwcalike(corp, dictionary=data_dictionary_LoughranMcDonald)
    sent2 = dplyr::select(sent2, negative:`modal words strong`)

    sent = cbind(sent, sent1, sent2)

    new = cbind(val, pol, sent)
    zv = apply(new,2,function(x)length(unique(x))==1)
    new = new[,!zv]
    names(new) = paste0( "sent_", names(new) )
    #add = new[,!names(new)%in%names(all.feats)]
    all.feats = add_features( all.feats, new )

    # Need sentence WC to make this feature.
    all.feats$WCperChar = all.feats$sent_WC / nchar(clean)
  }

  if ( !is.null( all.feats ) && clean_features ) {
    #all.feats = clean_features( all.feats,
                                #ignore = ignore,
                                #preProc = preProc )
    all.feats = do.call(rcttext::clean_features, c(list(meta=all.feats, ignore=ignore),
                                          preProc))
  }

  if (!is.null(terms)){
    vcat( verbose, "Generating term features" )
    dfm.freq = quanteda::dfm(quanteda::tokens(clean_text(raw)))

    f_terms <- terms %in% colnames(dfm.freq)
    if ( !all( f_terms ) ) {
      warning( "Not all terms (", paste0( terms[f_terms], sep=", " ), ") found in dfm" )
    }

    tmp=as.matrix(dfm.freq)[, which(colnames(dfm.freq)%in%terms), drop=FALSE]

    if (ncol(tmp) == 1) {
      tmp = as.data.frame(tmp)
      rownames(tmp)=NULL
      names(tmp)=terms
    }
    if ( ncol(tmp) > 0 ) {
      all.feats = add_features( all.feats, as.data.frame(tmp) )
    }
  }

  return(all.feats)
}



