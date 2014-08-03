#' @title Define defaults for lattice plotting
#' @description Define defaults for lattice plotting (cf. settrellis in the Hmisc library)
#' @usage
#'   set.trellis(col=c(8,6,6,6),pch=c(1,16))
#'   xyplot(I(rnorm(100))~rnorm(100))#' @name set.trellis
#' @author Claus E. Andersen
#' @return void. The side effect is that the trellis/lattice parameters have changed.
#' @param col is the vecor of colors
#' @param lty is the vector of line types
#' @param lwd is the vector of line widths
#' @param cex is the vector of symbol sizes
#' @param font is the font
#' @param what is "all" or "plot.line"
#' @param trace: If true the function becomes verbose.
#' @export set.trellis
set.trellis <- function(col=c(6,4,8,1,2,5,3),lty=1,lwd=1,pch=16,cex=0.8,font=1,what="all",trace=FALSE){
  # Library: clan
  # Task   : Define defaults of trellis (see settrellis in Hmisc)
  # Observe: set.trellis can be called more than once
  # Created: Dec. 29, 2003
  # Revised: Feb. 15, 2004
  # Revised: November 20, 2011 for R (no changes)
  # Sample call:
  # set.trellis() # Reset sellting
  # set.trellis(col=c(8,6,6,6,6,6,6,6,6,6,6,6)) # Set first col to be red and next ones to be blue
  # set.trellis(col=c(8,6,6,6),pch=c(1,16))
  # xyplot(I(rnorm(100))~rnorm(100))
  # show.settings()
  
  if(is.element(what,c("all","plot.symbol"))) {
    # ordinary xyplot etc.
    plot.symbol <- trellis.par.get("plot.symbol")
    plot.symbol$col <- col[1]
    plot.symbol$pch <- pch[1]
    plot.symbol$cex <- cex[1]
    plot.symbol$font <- font[1]
    if(trace) print(plot.symbol)
    trellis.par.set("plot.symbol",plot.symbol)
  }
  
  if(is.element(what,c("all","plot.line"))) {
    # ordinary xyplot etc.
    plot.line<- trellis.par.get("plot.line")
    plot.line$col <- col[1]
    plot.line$lty <- lty[1]
    plot.line$lwd <- lwd[1]
    if(trace) print(plot.line)
    trellis.par.set("plot.line",plot.line)
  }
  
  if(is.element(what,c("all","superpose.symbol"))) {
    # superpose
    ss <- trellis.par.get("superpose.symbol")
    max.length <- max(length(ss$col),length(ss$pch),length(ss$cex),length(ss$font))
    max.length <- max(length(col),length(pch),length(cex),length(font))
    if(length(cex)<1){ ss$cex[] <-cex }  else { N <- ceiling(max.length/length(cex));  ss$cex  <- rep(cex,N)}
    if(length(col)<1){ ss$col[] <-col }  else { N <- ceiling(max.length/length(col));  ss$col  <- rep(col,N)}
    if(length(pch)<1){ ss$pch[] <-pch }  else { N <- ceiling(max.length/length(pch));  ss$pch  <- rep(pch,N)}
    if(length(font)<1){ss$font[]<-font } else { N <- ceiling(max.length/length(font)); ss$font <- rep(font,N)}
    ss$cex <- ss$cex[1:max.length]
    ss$col <- ss$col[1:max.length]  
    ss$pch <- ss$pch[1:max.length]	
    ss$font <-ss$font[1:max.length]
    if(trace) print(ss)
    trellis.par.set("superpose.symbol",ss)
  } 
  
  if(is.element(what,c("all","superpose.line"))) {
    # superpose
    sl <- trellis.par.get("superpose.line")
    max.length <- max(length(sl$col),length(sl$lty),length(sl$lwd))
    max.length <- max(length(col),length(lty),length(lwd))
    if(length(col)<1){ sl$col[] <-col }  else { N <- ceiling(max.length/length(col));  sl$col  <- rep(col,N)}
    if(length(lty)<1){ sl$lty[] <-lty }  else { N <- ceiling(max.length/length(lty));  sl$lty  <- rep(lty,N)}
    if(length(lwd)<1){ sl$lwd[] <-lwd }  else { N <- ceiling(max.length/length(lwd));  sl$lwd  <- rep(lwd,N)}
    sl$col <- sl$col[1:max.length]
    sl$lty <- sl$lty[1:max.length]	
    sl$lwd <- sl$lwd[1:max.length]	
    if(trace) print(sl)
    trellis.par.set("superpose.line",sl)
  } 
  
  invisible()
} # end set.trellis
