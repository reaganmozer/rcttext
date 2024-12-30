


#' Plot the results from a CCS run
#'
#' This function provides a visualization of the set of words and phrases
#' found to differ systematically between treatment and control groups
#'
#'
#' @param out a \link{textreg.result()} object
#' @param xlim limits for x-axis
#' @param xadj adjustments to the lower and upper limits on the x-axis of the plot
#' @param ... additional arguments passed to plot
#' @export
plot_ccs = function(out, xlim=NULL, xadj=c(-0.025,0.025),
                    xlab="Difference in Usage Rates", sub="(Treatment - Control)", ...){
  out1 = out
  xlim0 = round(c(min(out1$diff.val), max(out1$diff.val)),1)


  if (is.null(xlim)){
    xlim = xlim0
  }

  xlim=xlim+xadj

  xvals = seq(xlim[1], xlim[2], length.out = nrow(out1))
  yvals = sample(seq(0,10, length.out=nrow(out1)))

  out1$tot.avg = (out1$tot1+out1$tot0)/(out1$ntreat+out1$ncontrol)

  par(mar=c(5.6,0.5,2.1,0.5))
  plot(x=xvals,y=yvals, xlim=xlim,ylim=c(0,10),ylab="",
       yaxt="n",xlab="",xaxt="n",cex.main=1.1,
       type="n",...)
  abline(v=0,lty=2)
  text(x=out1$diff.val, y=yvals, labels=out1$phrase, cex=0.75+2*out1$tot.avg)#cex=0.6+(out1$n.mods)/4)
  axis(side=1)
  title(xlab=xlab, sub=sub,
        cex.lab=0.95, cex=0.8)
}
