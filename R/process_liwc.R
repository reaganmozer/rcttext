#' Internal functions for processing LIWC output



get_dimnames = function(){
  fnames=read.csv("data/dimnames.csv")[,-c(1)]
  names(fnames)=c("name","fname")
  return(fnames)

}
