
#' MORE study science essays
#'
#' Dataset consisting of a sample of 40 student essays and corresponding human-coded quality scores (sampled from MORE study pilot data)
#' @keywords data
#' @format A \link{data.frame} with 40 observations of 4 variables:
#' \describe{
#' \item{studentid}{Student identification number}
#' \item{text}{Student generated essay text}
#' \item{treatment}{MORE intervention treatment indicator (0=Control, 1=Treatment)}
#' \item{score}{Human-coded writing quality score}
#' }
#'
"student_essays"



#' Dataset with 20 essays from READS pilot data
#'
#' Used for testing and illustation of rcttext functions.
#' @details 5 column data.frame, ID is the id of subject, Q1, Q2, more are meta information on scoring, and text contains character string of the text of the essay.
#' @keywords data
#' @format A \link{data.frame} with 5 columns and 20 rows
#'
"toy_reads"





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
