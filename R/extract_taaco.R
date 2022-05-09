#' Manage and merge text features generated using TAACO
#'
#' Tools to support feature extraction using TAACO.
#' \code{prep_taaco()} prepares a corpus for analysis in TAACO.
#' \code{extract_taaco()} reads output and log files produced by TAACO program and returns a \code{data.frame} that can be merged with other feature sets.
#'
#' @param file Filename where TAACO results are stored
#' @param data Optional \code{data.frame} with additional document-level variables to include in output.
#' @param idvar If \code{data} is specified, character vector with name(s) of variables used for merging.
#' @return Returns a \code{data.frame} of text features.
#' @export
extract_taaco <- function(file, data=NULL, idvar=NULL){


}

# @import fs
# @import progress
#' @rdname extract_taaco
#' @param x A [quanteda::corpus()] object or character vector of text documents.
#' @param dir Name of directory where TAACO intermediate text files should be stored.
#' @param docnames Optional character string specifying file names for each document in \code{x}.

prep_taaco = function(x,dir,docnames=NULL){


  library(tm)
  library(fs)
  library(progress)

  pb = progress::progress_bar$new(format="writing text files [:bar] :percent",clear=FALSE, total=nrow(text),width=60)
  fs::dir_create(dir)

  if (is.null(docnames)){docnames=1:length(x)}
  fname.out = paste0(dir, docnames)

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
