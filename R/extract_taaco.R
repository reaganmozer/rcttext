#' Manage and merge text features generated using TAACO
#'
#' These methods help support feature extraction using TAACO.  You
#' will have to use the external TACCO program to generate these
#' features; these methods just help move back and forth from R to
#' TACCO.
#'
#' \code{prep_taaco()} prepares a corpus for analysis in TAACO.  Call
#' this on the corpus to make files that can be read in and processed
#' easily.
#'
#' Once external processing is complete, \code{extract_taaco()} reads
#' output and log files produced by the TAACO program and returns a
#' \code{data.frame} that can be merged with other feature sets.
#'
#' The "Filename" column in the read file should be the IDs (with a
#' '.txt' suffix that will be dropped).  The results can then be
#' merged with `meta`, if passed,
#'
#' @param file Filename where TAACO results are stored
#' @param meta Optional \code{data.frame} with additional
#'   document-level variables to include in output.
#' @param ID.meta If \code{meta} is specified, character vector with
#'   name of variables used for merging.
#' @param drop_para Drop paragraph level measures of cohesion from
#'   features (TRUE/FALSE).
#' @param drop_sent Drop adjacent overlap  between sentences
#'   (TRUE/FALSE).
#' @return Returns a \code{data.frame} of text features.
#' @export
extract_taaco <- function(file, meta=NULL, ID.meta=NULL,
                          drop_para = FALSE, drop_sent = TRUE ) {


  add = read.csv(file)

  add$ID = as.numeric(gsub(".txt", "", add$Filename))

  if ( drop_para ) {
    # remove paragraph-level measures of cohesion
    add = add[,-c(grep("para", names(add)))]
  }

  if ( drop_sent ) {
    # remove adjacent overlap  between sentences
    add = add[,-c(grep("adjacent",names(add)))]
  }

  add = dplyr::select(add, ID, everything(), -Filename)

  not_id = colnames(add) != "ID"
  colnames(add)[not_id] = paste0( "taaco_", colnames(add)[not_id] )

  if ( !is.null( meta ) ) {
    meta = merge(meta, add,
                 by.x=ID.meta, by.y="ID", all.x=TRUE )
  } else {
    meta = add
  }

  return( meta )

}

#' Prepare TAACO files for later import into the tada universe.
#'
#' @rdname extract_taaco
#' @param x A [quanteda::corpus()] object or character vector of text documents.
#' @param dir Name of directory where TAACO intermediate text files should be stored.
#' @param docnames Optional character string specifying file names for each document in \code{x}.
#' @import fs
#' @import progress
#' @export
prep_taaco = function(x, dir, docnames=NULL){


  library(tm)
  library(fs)
  library(progress)

  pb = progress::progress_bar$new(format="writing text files [:bar] :percent",
                                  clear=FALSE, total=nrow(text),
                                  width=60)
  fs::dir_create(dir)

  if (is.null(docnames)){
    docnames=1:length(x)
  }
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
