#' @title Demonstration of the use of panel.binned.errorbars for Lattice graphics
#' @description Demonstration of the use of panel.binned.errorbars for Lattice graphics.
#' See also \link{panel.binned.errorbars}, \link{panel.ebars} and \link{panel.ebars2}.
#' @usage
#' panel.binned.errorbars.demo()
#' @name panel.binned.errorbars.demo
#' @author Claus E. Andersen
#' @return The main output is the sideeffect of creating a bunch of lattice plots
#' @param dat is not used for anything
#' @param dat.ok is not used for anything
#' @param return.synthetic.data is not used for anything
#' @export panel.binned.errorbars.demo
panel.binned.errorbars.demo <- function(dat, dat.ok=FALSE, return.synthetic.data=FALSE){
  # Purpose: Show how panel.binned.errorbars.examples is used
  # Created: October 17, 2005
  # Revised: October 17, 2005
  # Revised: November 12, 2011 (moved from S-plus to R)
  # Name:    Claus E. Andersen
  
  # Sampel call:
  #   panel.binned.errorbars.demo()
  
  # Create some test data
  N <- 100
  df <- data.frame(
    sec         = seq(0,N/10,length=N),
    counts      = rnorm(N),
    expid       = sample(c("Test 1","Test 2", "Test 3"),N,replace=T),
    temperature = sample(c(20,0.25,0,10,-5),N,replace=T))
  
  # Modify data fro Test 1
  ii <- df$expid=="Test 1"
  df$counts[ii] <- df$sec[ii]*2 + df$counts[ii]
  
  ii <- df$temperature==0
  df$counts[ii] <- -df$sec[ii]*2 + 5*df$counts[ii]
  
  #ii <- df$expid=="Test 2"
  #df$counts[ii] <- NA
  
  
  
  plt <- xyplot(counts~temperature,
                groups=factor(temperature),
                main="Plot 1 - the raw data",
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)),
                groups=factor(temperature),
                main="Plot 2 - separated using codes",
                auto.key=list(space="right"), data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)),
                groups=expid,
                main="Plot 3 - default binned.errorbars (mean + std.dev. of mean)",
                panel=function(x,y,...,groups,subscripts){
                  panel.binned.errorbars(x,y,subscripts,groups)
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)),
                groups=expid,
                main="Plot 4 - offset the errorbars +/-5 mm",
                panel=function(x,y,...,groups,subscripts){
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(x.offset=c(-5,0,5)))
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)),
                groups=expid,
                main="Plot 5 - show the standard deviation",
                panel=function(x,y,...,groups,subscripts){
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(show=F, show2=T, x.offset=c(-5,0,5)))
                },
                auto.key=list(space="right"),data=df)
  
  plt <- xyplot(counts~as.numeric(factor(temperature)),
                groups=expid,
                main="Plot 6 - show both standard deviation and std.dev of mean",
                panel=function(x,y,...,groups,subscripts){
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(show=T, show2=T, x.offset=c(-5,0,5)))
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)),
                groups=expid,
                main="Plot 7 - pool groups (x.offset=0)",
                panel=function(x,y,...,groups,subscripts){
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(pool.groups=T, show=T, show2=T, x.offset=c(0)))
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)),
                groups=expid,
                main="Plot 8 - include raw data (x.offset=-5)",
                panel=function(x,y,...,groups,subscripts){
                  panel.superpose(x,y,subscripts,groups,...)
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(pool.groups=T, show=T, show2=T, x.offset=c(-5)))
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)),
                groups=expid,
                main="Plot 9 - show min, max and median",
                panel=function(x,y,...,groups,subscripts){
                  panel.superpose(x,y,subscripts,groups,...)
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(pool.groups=T, show=F, show2=T, x.offset=c(-5),
                                                          y.center.function   = function(x){median(x,na.rm = T)},
                                                          y.low2.function     = function(x){min(x,na.rm = T)},
                                                          y.high2.function    = function(x){max(x,na.rm = T)}
                                         ))
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)),
                groups=expid,
                main="Plot 10 - divide into groups again",
                panel=function(x,y,...,groups,subscripts){
                  panel.superpose(x,y,subscripts,groups,...)
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(pool.groups=F, show=F, show2=T, x.offset=c(-5,-5,5),
                                                          y.center.function   = function(x){median(x,na.rm = T)},
                                                          y.low2.function     = function(x){min(x,na.rm = T)},
                                                          y.high2.function    = function(x){max(x,na.rm = T)}
                                         ))
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)) | expid,
                groups=expid,
                main="Plot 11 - divide into experiments",
                panel=function(x,y,...,groups,subscripts){
                  panel.superpose(x,y,subscripts,groups,...)
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(pool.groups=F, show=F, show2=T, x.offset=c(-7,-7,-7),
                                                          y.center.function   = function(x){median(x,na.rm = T)},
                                                          y.low2.function     = function(x){min(x,na.rm = T)},
                                                          y.high2.function    = function(x){max(x,na.rm = T)}
                                         ))
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)) | expid,
                groups=expid,
                main="Plot 12 - change colors etc.",
                panel=function(x,y,...,groups,subscripts){
                  panel.superpose(x,y,subscripts,groups,...)
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(pool.groups=F, show=F, show2=T, x.offset=c(-7,-7,-7),
                                                          col.line2='green',col.bar2='red',
                                                          y.center.function   = function(x){median(x,na.rm = T)},
                                                          y.low2.function     = function(x){min(x,na.rm = T)},
                                                          y.high2.function    = function(x){max(x,na.rm = T)}
                                         ))
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)) | expid,
                groups=expid,
                main="Plot 13 - negative colors => use point colors for eror bars",
                panel=function(x,y,...,groups,subscripts){
                  panel.superpose(x,y,subscripts,groups,...)
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(pool.groups=F, show=F, show2=T, x.offset=c(-7,-7,-7),
                                                          col.line2=-1,col.bar2=-1,
                                                          y.center.function   = function(x){median(x,na.rm = T)},
                                                          y.low2.function     = function(x){min(x,na.rm = T)},
                                                          y.high2.function    = function(x){max(x,na.rm = T)}
                                         ))
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)) | expid,
                groups=expid,
                main="Plot 14 - Keep some space free around the center point",
                panel=function(x,y,...,groups,subscripts){
                  panel.superpose(x,y,subscripts,groups,...)
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(pool.groups=F, show=F, show2=T, x.offset=c(-7,-7,-7),
                                                          col.line2=-1,col.bar2=-1,
                                                          point.radius2=3,
                                                          y.center.function   = function(x){median(x,na.rm = T)},
                                                          y.low2.function     = function(x){min(x,na.rm = T)},
                                                          y.high2.function    = function(x){max(x,na.rm = T)}
                                         ))
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)),
                groups=expid,
                main="Plot 15 - show the number of measurements",
                panel=function(x,y,...,groups,subscripts){
                  panel.abline(h=0,lty='dashed')
                  #panel.superpose(x,y,subscripts,groups,...)
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(pool.groups=F, show=F, show2=T, x.offset=c(-7,0,7),
                                                          col.line2=-1,col.bar2=-1,
                                                          point.radius2=1,
                                                          y.center.function   = function(x){median(x,na.rm = T)},
                                                          y.low2.function     = function(x){min(x,na.rm = T)},
                                                          y.high2.function    = function(x){max(x,na.rm = T)},
                                                          y.lab.function   = function(x){max(x,na.rm = T)},
                                                          x.offset.lab=3, y.offset.lab.center=-5, cex.lab=0.5, srt.lab=0, col.lab=1, adj.lab=0,
                                                          show.lab.N=T,
                                                          show.lab.center=F,
                                                          round.lab=5,
                                                          lab.code  = paste("N=length(x.center)")
                                         ))
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
  
  plt <- xyplot(counts~as.numeric(factor(temperature)),
                groups=expid,
                main="Plot 16 - show both number of measurements and center values",
                panel=function(x,y,...,groups,subscripts){
                  panel.abline(h=0,lty='dashed')
                  #panel.superpose(x,y,subscripts,groups,...)
                  panel.binned.errorbars(x,y,subscripts,groups,
                                         err.control=list(pool.groups=F, show=F, show2=T, x.offset=c(-7,0,7),
                                                          col.line2=-1,col.bar2=-1,
                                                          point.radius2=1,
                                                          y.center.function   = function(x){median(x,na.rm = T)},
                                                          y.low2.function     = function(x){min(x,na.rm = T)},
                                                          y.high2.function    = function(x){max(x,na.rm = T)},
                                                          y.lab.function   = function(x){max(x,na.rm = T)},
                                                          x.offset.lab=3, y.offset.lab.center=-5, cex.lab=0.5, srt.lab=0, col.lab=1, adj.lab=0,
                                                          show.lab.N=T,
                                                          show.lab.center=T,
                                                          round.lab=5,
                                                          lab.code  = paste("N=length(x.center)")
                                         ))
                },
                auto.key=list(space="right"),data=df)
  plot(plt)
  
} # End panel.binned.errorbars.demo
