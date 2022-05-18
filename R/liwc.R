#' Functions for processing and appending output from Linguistic Inquiry Word Count (LIWC) software
#'
#' @param file character path to LIWC output file
#' @param x optional data frame to append LIWC output
#' @param by if x is specified, character vector of the columns used for merging
#' @param clean should LIWC output be cleaned prior to appending (e.g., remove linear combinations, repeat variables, etc.)
#' @export

liwc = function(file, x=NULL, by=NULL, clean=T){
  liwc=read.csv(file,header=T)
  tmp = dplyr::select(liwc, !WC:OtherP)

  check = sum(liwc[1,13:31])
  if (check==0){
    ind = which(names(liwc)%in%names(tmp))
    names(liwc)[ind]=liwc[1,ind]
    liwc=liwc[-c(1),]
  }
  liwc = dplyr::select(liwc, by, WC:OtherP)


  if (!is.null(x)){
    liwc2=dplyr::select(liwc, !names(liwc)[names(liwc)%in%names(x)], by)

    out = merge(x, liwc2,  by=by)
  }
  else if (is.null(x)){
    out = liwc
  }

  if (clean){
    out2=dplyr::select(out, -by)
    lc = caret::findLinearCombos(out2)
    if (!is.null(lc$remove)){ out = out2[,-c(lc$remove)] }
  }
  return(out)

}




get_dimnames = function(){
  fnames=read.csv("data/dimnames.csv")[,-c(1)]
  names(fnames)=c("name","fname")
  return(fnames)

}
