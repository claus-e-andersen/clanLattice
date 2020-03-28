#' @title Demonstration of how to use panel.ebars for drawing error bars in lattice plots
#' @description Demonstration of how to use panel.ebars for drawing error bars in lattice plots
#' See also \link{panel.binned.errorbars}, \link{panel.ebars} and \link{panel.ebars2}.
#' @usage
#' panel.ebars.demo()
#' @name panel.ebars.demo
#' @author Claus E. Andersen
#' @return Lattice plot examples with error bars
#' @export panel.ebars.demo
panel.ebars.demo <- function(){
  # Created: May 20, 2012
  # Revised: May 22, 2012
  # Demonstration of how to use panel.ebars for drawing error bars in lattice plots
  ###########################################################
  df <- data.frame(Gy=1:10, cm.coordinate= -5*(1:10),film.no=1,Gy.err=2,exp=c(rep("exp1",5),rep("exp2",5)))
  df$Gy.err=df$Gy*0.1 
  
  xx <- rep(1.5,nrow(df))
  xx[1:3] <- NA
  xx[4] <- 10
  
  # Without groups (just use panel.xyplot)
  plt <- xyplot(Gy ~ cm.coordinate| exp , data=data.frame(df), subset=cm.coordinate>-40,
                main = list("Example with different whisker widths and double points: one from from xyplot + one (blue) point from panel.ebars.",cex=0.8), 
                x.minus=rep(5.5,nrow(df)),x.plus=xx,
                y.plus=3*df$Gy.err, y.minus=1*df$Gy.err,
                panel=function(x,y,subscripts,...){
                  panel.ebars(x,y,subscripts, x.width=4, y.width=10,point.wanted=TRUE, gp.point=gpar(col='blue',lwd=3,cex=0.7),pch=16,...)
                  panel.xyplot(x,y,col='red',type="p",pch=1,cex=2,...)
                },aspect=1)
  print(plt)
  
  
  # With groups (use panel.superpose)
  plt <- xyplot(Gy ~ cm.coordinate | exp, data=data.frame(df), subset=cm.coordinate>-40,
                main = list("Example with two sets of y-error bars, where one is offset relative to the other.",cex=0.8), 
                groups=exp,
                x.minus=rep(5.5,nrow(df)),x.plus=xx,
                y.plus=3*df$Gy.err, y.minus=1*df$Gy.err,
                panel=function(x,y,subscripts,groups,...){
                  panel.ebars(x,y,subscripts, 
                              y.wanted=TRUE,x.wanted=FALSE,point.wanted=FALSE,
                              x.offset=0,y.offset=0,x.inner=0,x.width=3,y.width=3,y.inner=0,...)
                  
                  panel.ebars(x,y,subscripts,x.err=rep(0.2,nrow(df)),y.err=rep(0.2,nrow(df)), 
                              y.wanted=TRUE,x.wanted=FALSE,point.wanted=FALSE,
                              x.offset=0,y.offset=3,x.inner=0,x.width=3,y.width=3,y.inner=0)
                  
                  panel.superpose(x,y,subscripts,groups,...)
                },
                aspect=1)
  print(plt)
  
  'ByeBye'
} # panel.ebars.demo
