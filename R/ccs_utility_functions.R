

scat = function( str, ... ) {
  cat( sprintf( str, ... ) )
}



#' Find threshold C and run textreg with that tuning parameter.
#'
#' @param ... Additional arguments to pass to textreg.  See
#'   textreg::textreg for more information.
#' @param beta The quantile of the null distribution to use for
#'   regularization.
#' @param R The number of randomizations to use for the null
#'   distribution.
#'
#' @return Textreg result object with the passed tuning parameter.
#' @export
ccs_tuned_textreg <- function(corpus, Z, R = 20, beta = 0.80, ...) {
  require(textreg)

  if ( all( c( 0, 1 ) %in% unique( Z ) ) ) {
    Z = 2 * Z - 1
  }

  C <- find.threshold.C(corpus, Z, R = R)
  C_L2 <- quantile(C, beta)

  # Convert ... to a list and modify verbosity if needed
  args <- list(...)
  if (!("verbosity" %in% names(args))) {
    args$verbosity <- 0
  }

  scat("L2 C: %.2f / %.2f\n", C[[1]], C_L2)

  res1 <- do.call(textreg::textreg,
                  c( list( corpus = corpus,
                           labeling = Z,
                           C = C_L2 ),
                     args) )
  res1
}


#' Given a list of CCS models, produce a table of the models
#'
#' This is a wrapper of the textreg make.list.table() method with some
#' extra cleanup.
#' @param model_list A list of CCS models, or the result of a
#'   ccs_list_table() call.
#'
#'
#' @export
ccs_list_table <- function( model_list ) {

  # Hack to drop trailing * due to poor implementation, sadly, of
  # textreg.
  for ( i in 1:length(model_list) ) {
    model_list[[i]]$model$ngram = gsub( " \\*$", "", model_list[[i]]$model$ngram )
  }

  if ( is.null( names( model_list ) ) ) {
    names(model_list) = paste0( "model", 1:length(model_list) )
  }

  Cs = map_dbl( model_list, \(.x) .x$notes$C )

  tbl = textreg::make.list.table( model_list,
                                  model.names = paste( names(model_list),
                                                       round( Cs, digits=1 ),
                                                       sep="-" ),
                                  method = "rank" )

  tbl
}


#' Given a list of CCS model, produce a summary table of the results
#'
#' @inheritParams ccs_list_table
#'
#' @param corpus The corpus (as a list of strings)
#' @param Z The binary treatment variable
#' @return A data frame containing summary of the results.
#' @export
ccs_result_table = function( model_list, corpus, Z) {

  result = model_list
  R = 0
  if ( !is.data.frame( model_list ) ) {
    R = length( model_list )
    result = ccs_list_table( model_list )
  } else {
    R = which( colnames( model_list ) == "num.phrase" ) - 2
  }

  result$n.mods = sapply(1:nrow(result),function(x) sum(!is.na(result[x,c(2:(R+1))])))
  tmp = subset(result,select=c(phrase, n.mods, num.reports, num.tag))
  tmp$count.neg = tmp$num.reports-tmp$num.tag
  names(tmp)=c("phrase","n.mods","docs.total", "docs1","docs0")
  tmp2 = tmp %>% mutate(prop.docs1= docs1/sum(Z),
                        prop.docs0 = docs0/sum(1-Z),
                        prop.diff = prop.docs1-prop.docs0)


  phrases = unique(tmp2$phrase)
  m = make.phrase.matrix(phrases, corpus)
  tmp2$phrase.tot = colSums(m)
  tmp2$tot1 = colSums(m[Z==1,])
  tmp2$tot0 = colSums(m[Z==0,])

  out = select(tmp2, phrase, n.mods, tot1, tot0,prop.diff)
  out$docs1 = paste0(tmp2$docs1, " (",round(tmp2$prop.docs1,2)*100, "%)")
  out$docs0 = paste0(tmp2$docs0, " (",round(tmp2$prop.docs0,2)*100, "%)")
  out$diff.val=tmp2$prop.diff
  out$diff = paste0(round(tmp2$prop.diff,2)*100,"%")
  out$diff[tmp2$prop.diff>0]=paste0("+",out$diff[tmp2$prop.diff>0])
  #out$docs.diff = paste0(round(tmp2$prop.diff*100,1),"%")
  out = out[with(out,order(desc(n.mods),desc(prop.diff))),]
  out = select(out, phrase, n.mods, tot1, tot0,docs1, docs0, diff, diff.val)
  rownames(out)=NULL

  ntreat = sum( Z == 1 )
  out$ntreat = ntreat
  out$ncontrol = sum( Z != 1 )

  out
}

