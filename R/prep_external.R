




#' Prepare text documents for analysis using external programs
#'
#' Text pre-processing and corpus management functions to provide
#' compatibility with external text analysis programs and standalone
#' software packages such as Linguistic Inquiry Word Count (LIWC), the
#' Tool for Automated Analysis of Cohesion (TAACO) and the Sentiment
#' Analysis and Social Cognition Engine (SEANCE).
#'
#' @param x A \link{corpus} object or character vector of text
#'   documents.
#' @param dir Name of directory where the generated intermediate text
#'   files should be stored.
#' @param docnames Optional character string specifying file names for
#'   each document in \code{x}.
#' @param preProc Optional text pre-processing function(s) (e.g.,
#'   stemming) to apply prior to writing text files for analysis in
#'   external programs.
#' @import fs
#' @import progress
#'
#' @references{ \insertRef{liwc2015}{rcttext}
#' \insertRef{crossley2016taaco}{rcttext}
#' \insertRef{crossley2017sentiment}{rcttext}}
#' @export

prep_external = function(x, dir, docnames=NULL, preProc=NULL){


  library(tm)
  library(fs)
  library(progress)

  pb = progress::progress_bar$new(format="writing text files [:bar] :percent",clear=FALSE, total=length(x),width=60)
  fs::dir_create(dir)

  if (is.null(docnames)){docnames=1:length(x)}
  fname.out = paste0(dir, docnames, ".txt")

  # Create a text file for each essay to analyze via TAACO
  for (j in 1:length(x)){
    fileConn = file(fname.out[j])
    writeLines(x[j], fileConn)
    close(fileConn)
    pb$tick()
    Sys.sleep(1/100)
  }
  pb$terminate()
}


