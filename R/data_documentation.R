


#' Dataset with 20 essays from READS pilot data
#'
#' Used for testing and illustation of tada functions.
#' @details 5 column data.frame, ID is the id of subject, Q1, Q2, more are meta information on scoring, and text contains character string of the text of the essay.
#' @keywords data
#' @format A \link{data.frame} with 5 columns and 20 rows
#'
"toy_reads"



#' Dimension names of LIWC/TAACO that are more accessible
#'
#' @docType data
#' @details Pretty-fied dimension names for LIWC/TAACO features
#' @keywords data
#' @format A \link{data.frame} with 36 rows and 2 columns
"dimnames"




#' Mini glove dataset
#'
#' glove embeddings (50 dimensional) for 1000 common words beyond
#' those words listed in several stopword lists provided by quanteda.
#'
#' Original glove embeddings downloaded from Stanford CITE.
#'
#' @docType data
#' @details 1000 by 50 matrix, each row is an embedding.  Rownames are
#'   words.
#' @keywords data
#' @format A \link{matrix} with 1000 rows and 50 columns
"mini_glove"
