% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_features.R
\name{generate_features}
\alias{generate_features}
\title{Generate an array of text features}
\usage{
generate_features(
  x,
  meta = NULL,
  lex = TRUE,
  sent = TRUE,
  ld = "all",
  clean_features = TRUE,
  read = c("ARI", "Coleman", "DRP", "ELF", "Flesch", "Flesch.Kincaid",
    "meanWordSyllables"),
  terms = NULL,
  preProc = list(uniqueCut = 1, freqCut = 99, cor = 0.95, remove.lc = TRUE),
  verbose = FALSE,
  ignore = NULL,
  ...
)
}
\arguments{
\item{x}{A \link{corpus} object or character vector of text
documents.}

\item{meta}{Dataframe corresponding to the corpus x.  If passed,
and non-NULL, all generated features will be added to this
dataframe.  If NULL, a dataframe of just the features will be
returned.}

\item{lex}{Logical, indicating whether to compute lexical indices
including measures of lexical diversity, readability, and entropy}

\item{sent}{Logical, indicating whether to compute sentiment
analysis features from available dictionaries}

\item{ld}{character vector defining lexical diversity measures to
compute; see \link{quanteda.textstats::textstat_lexdiv()}}

\item{clean_features}{TRUE means implement cleaning step where
features with no variation and collinear features are dropped.
(This happens before any term generation features are added.)}

\item{read}{character vector defining readability measures to
compute; see \link{quanteda.textstats::textstat_readability()}}

\item{terms}{character vector of terms to evaluate as standalone
features based on document-level frequency (case-insensitive).
Not cleaned by clean_features.}

\item{preProc}{Named list of arguments passed to
\code{clean_features()} for applying pre-processing
transformations across the set of text features (e.g., removing
collinear features).}

\item{ignore}{List of column names (features) to ignore when
simplifying (e.g., ID column and other columns that should be
preserved).  All columns in meta, if meta is non-null, will be
added to this list automatically.}

\item{...}{(optional) additional arguments passed to
\link{quanteda::tokens()} for text pre-processing.}
}
\value{
A data.frame of available text features, one row per
  document, one column per feature (with additional columns from
  meta, if meta is not NULL).
}
\description{
Generates a rich feature representation for documents provided as a
character vector or \link{quanteda::corpus()} object by applying an
array of linguistic and syntactic indices, available text analysis
dictionaries, and pre-trained embedding models to all documents.
}
\details{
Note: This function does not work with a dataframe with one row.
The 'meta' argument requires a dataframe with at least two rows.
}
\examples{


## Example 1: Basic Feature Generation

# Create a small dataframe with 2 texts (objects).
df = data.frame(
 text = c( "This function generates an array of text features",
           "This function generates a rich feature representation as a character
            vector or quanteda::corpus() object by applying an array of linguistic
            and syntactic indices" ))

# Generated text features without simplifying the set of features
feats1 = generate_features( df$text,
                            meta = df,
                            clean_features = FALSE )


## Example 2: Feature Generation with Example Data and Customization

# Load example dataframe with multiple texts
data( "toy_reads" )

# Generate text features without simplifying the set of features
feats2 = generate_features( toy_reads$text,
                            meta=toy_reads,
                            clean_features = FALSE,
                            ignore = "ID" )

# Generate preliminary text features,
# simplifying the set of features and specifying sent, read, ld
feats3 = generate_features( toy_reads$text, meta=toy_reads,
                            sent = TRUE,
                            clean_features = TRUE,
                            read = c("Flesch","Flesch.Kincaid", "ARI"),
                            ld=c("TTR","R","K"),
                            ignore=c("ID"),
                            verbose = TRUE )

}
