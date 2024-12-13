#' Functions for processing and appending output from Linguistic
#' Inquiry Word Count (LIWC) software
#'
#' @param file character path to LIWC output file
#' @param meta optional data frame to attach LIWC output to
#' @param ID.liwc ID column (either name or column number) of document
#'   IDs in the liwc file.
#' @param ID.meta If meta is specified, character vector of the
#'   document ID column to use for merging, corresponding with IDs in
#'   ID.liwc.
#' @param clean should LIWC output be cleaned prior to appending
#'   (e.g., remove linear combinations, repeat variables, etc.)
#' @export

extract_liwc <- function(file, meta=NULL, ID.liwc=1, ID.meta = NULL, clean=TRUE) {

  liwc=read.csv(file,header=TRUE)

  # WHAT IS THIS STUFF FOR???
  check = sum(liwc[1,13:31])
  if (check==0){
    tmp = dplyr::select(liwc, !WC:OtherP)

    ind = which(names(liwc)%in%names(tmp))
    names(liwc)[ind]=liwc[1,ind]
    liwc=liwc[-c(1),]
  }

  # Grab the features and (maybe) ID column
  if ( is.null( ID.liwc ) ) {
    liwc = dplyr::select(liwc, WC:OtherP)
  } else {
    if ( is.numeric( ID.liwc ) ) {
      ID.liwc = names(liwc)[ID.liwc]
    }
    liwc = dplyr::select(liwc, all_of( ID.liwc ), WC:OtherP)
  }

  # Merge to passed dataframe if one passed.
  if (!is.null(meta)){
    stopifnot( !is.null(ID.meta) )
    #stopifnot( length(ID.meta) == 1 )
      # Want to allow for matching on multiple columns

    # drop duplicate features
    liwc = dplyr::select(liwc, !names(liwc)[names(liwc)%in%names(meta)], any_of( ID.liwc ) )

    not_id = names(liwc) != ID.liwc
    names(liwc)[not_id] = paste0( "liwc_", names(liwc)[not_id] )

    out = merge(meta, liwc,  by.x=ID.meta, by.y = ID.liwc,
                all.x=TRUE, all.y = FALSE )
  } else {
    out = liwc
  }

  if (clean){ # I think this works, but it's currently breaking because of
              # the IDs not matching the LIWC file
    out2=dplyr::select(out, -all_of( ID.liwc, ID.meta) )
    lc = caret::findLinearCombos(out2)
    if (!is.null(lc$remove)){
      drops = colnames(out2)[ lc$remove ]
      out = dplyr::select( out, !any_of( drops ) )
    }
  }

  return(out)

}




get_dimnames = function(){
  fnames=read.csv("data/dimnames.csv")[,-c(1)]
  names(fnames)=c("name","fname")
  return(fnames)

}
