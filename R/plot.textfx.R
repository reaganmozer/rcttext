#' Plot the results from an impact analysis with text outcomes
#'
#' This function provides a visualization of the set of textual features
#' found to differ systematically between treatment and control groups.
#'
#' @import plotrix
#' @import scales
#'
#' @param x a model object output from \code{estimate_impacts()}
#' @param alpha the threshold for determining statistical significance
#' @param cols should effects be colored by direction (red for negative impacts, blue for positive impacts)
#' @param group (optional) should effects be grouped by category (e.g., higher-level summary measures, linguistic features, etc.)
#' @param ... additional arguments passed to plot
#' @export



plot.textfx= function(out,  alpha=0.05, cols=F, group=NULL, ...){

  ord = rep(3,nrow(out))
  if (!is.null(group) & group==TRUE){
    ord[out$name%in%c("Analytic","Authentic","Clout","Tone")]=1
    ord[out$name%in%c("WC","WPS","TTR","XXX","Sixltr",'Flesch.Kincaid', "Flesch",
                       "R","ARI")]=2
  }
  fnames=get_dimnames()
  out1 = merge(out, fnames, by="name",all.x=T)
  out1$fname[is.na(out1$fname)]=out1$name[is.na(out1$fname)]
  out1$name=out1$fname
  out1$ord=ord
  out1 = dplyr::arrange(out1, desc(ord), desc(name))

  if (cols){cols= ifelse(out1$est>0,"blue","red")}
  else{cols="gray"}
  if (!is.null(xlim)){
    xl=xlim[1]
    xr=xlim[2]
  }
  else{
    xl=min(out1$LL)-0.1
    xr=max(out1$UL)+0.1
  }

  plotCI(x=out1$est, y=1:nrow(out1),ui=out1$UL, li=out1$LL,
         err="x",yaxt="n",ylab="",xlab="Standardized difference in means (95% CI)",
         pch=NA,scol="white",xlim=c(xl-0.05,xr+0.05),xaxt="n",...)
  axis(side=1,at=seq(xl, xr, by=0.2),labels=sprintf("%0.1f",seq(xl,xr,0.2)))
  abline(v=0,lty=2)
  if (length(unique(out1$ord))>2){
    ind=c(min(which(out1$ord==1)),min(which(out1$ord==2)))-0.5
    abline(h=ind[1],col="darkgray",lty=3,lwd=2)
    abline(h=ind[2],col="darkgray",lty=3,lwd=2)
  }


  linecols = ifelse(out1$p.adj<=alpha, 1,0.5)
  ptcols = ifelse(out1$p.adj<=alpha, 0.75, 0.1)
  plotCI(x=out1$est, y=1:nrow(out1), ui=out1$UL, li=out1$LL,
         err="x",add=T,scol=alpha("black",linecols), col=alpha("black",linecols),
         pch=21, pt.bg=alpha(cols,ptcols), cex=1.3,lwd=1.25)

  which.sigs = out1$p.adj<=alpha
  y1 = (1:nrow(out1))[!which.sigs]
  y2 = (1:nrow(out1))[which.sigs]



  axis(side=2,at=y1,labels=out1$name[!which.sigs],las=2,cex.axis=0.875,tcl=-0.25)
  axis(side=2,at=y2,labels=out1$name[which.sigs],las=2,cex.axis=0.95,tcl=-0.25,
       font=2)

}
