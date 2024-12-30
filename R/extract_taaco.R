#' Import and merge text features generated using TAACO
#'
#' This method helps support feature extraction using TAACO.  You will
#' have to use the external TACCO program to generate these features;
#' these methods just help move back and forth from R to TACCO.
#'
#'
#' See \code{prep_external()} for generating text files that would be
#' ready for TAACO analysis. Call this on the corpus to make files
#' that can be read in and processed easily.
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
#'
#' @examples
#' # This function extracts features from a TAACO-generated data
#' # (CSV or dataframe).
#' # The txt_data should be a dataframe
#' # and have the same column name(s) as "reads_tacco"
#'
#' data("reads_taaco")
#' data("example_meta")
#'
#' reads_tacco = reads_taaco
#' txt_data = meta
#'
#' all.feats = extract_taaco( "reads_taaco",
#'                             meta = txt_data,
#'                             ID.meta = "ID" )
#'
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
