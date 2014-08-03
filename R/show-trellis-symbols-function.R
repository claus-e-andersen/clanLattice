#' @title Show available colors and symbols for Lattice graphics
#' @description Show the various colors and symbols in Trellis based on integer index
#' as given in palette().
#' @usage
#' show.trellis.symbols(palette(),"default palette")
#' @name show.trellis.symbols
#' @author Claus E. Andersen
#' @return plt A Lattice plot showing the colors
#' @param col.pal is the pallette Such as: 
#'    palette(cm.colors(10))
#'    palette(heat.colors(11))
#'    palette(terrain.colors(12))
#'    palette(topo.colors(12))
#'    palette("default"))
#' @param txt is the title of the plot
#' @param pch is a vector of symbols to be used
#' @param cex is the relative size of the symbols 
#' @export show.trellis.symbols
show.trellis.symbols <- function(col.pal=palette(), txt="", pch = 0:25, cex = 1.5){
  # Task   : Show the various colors and symbols in Trellis based on integer index
  #          as given in palette().
  # Library: clan
  # Created: November 11, 2002
  # Revised: November 11, 2002
  # Revised: September 18, 2011
  # Name   : Claus E. Andersen
  # Sample call:
  #   palette(cm.colors(10))
  #   palette(heat.colors(11))
  #   palette(terrain.colors(12))
  #   palette(topo.colors(12))
  #   palette("default")
  #   show.trellis.symbols(palette(),"default palette")
  x.col <- 1:length(col.pal)
  y.pch <- pch
  cex0 <- cex
  N.col <- length(x.col)
  N.pch <- length(y.pch)
  xx <- rep(x.col, N.pch)
  yy <- NULL
  for(i in y.pch){ yy <- c(yy, rep(i, N.col)) }
  plt <- xyplot(yy ~ xx, 
                ylim=c(-5,max(pch)+2),     
                scales = list(x = list(at = x.col), y = list(at = y.pch)), 
                xlab = "col", 
                ylab = "pch", 
                main = paste(txt,"(show.trellis.symbols)"), cex0 = cex0, 
                panel = function(x, y, cex0, ...){
                  for(k in 1:length(x)) {
                    lpoints(x[k], y[k], cex = cex0, pch = y[k], col= x[as.integer(k)],...)
                    ltext(x[k],-4.5,paste(col.pal[x[as.integer(k)]]),cex=.9,srt=90,adj=0)
                  }
                }
  ) # plt
  return(plt)
}
