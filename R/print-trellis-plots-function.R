#' @title Help place multiple lattice plots on a single page
#' @description Help place multiple lattice plots on a single page. Note that the
#' plots to be placed should be given as a list.
#' @usage
#'   plt0 <- xyplot(rnorm(10)~rnorm(10))
#'   print.trellis.plots(list(plt0,plt0,plt0),3,1)
#' @name print.trellis.plots
#' @author Claus E. Andersen
#' @return plt A Lattice plot showing the colors
#' @param plt.list is the list of trellis plots. NULL is ok, also for single elements in the list.
#' @param cols specify a given number of columns
#' @param rows specify a given number of rows
#' @param x.size is the x-region to be used (0-1)
#' @param y.size is he y-region to be used (0-1)
#' @param x.overlap is the amount of x-overlap
#' @param y.overlap is the amount of y-overlap
#' @param use.plt.void: Control if some plot should be made where elements in the plt.list are NULL
#' @param plt.void is the default void plot may be supplied
#' @param trace: If true, the function with provide text output during execution
#' @param more: If false, the last plot in this call is the final one (see print.trellis). Otherwise
#' it is assumed that more stuff will be added to the plot.
#' @export print.trellis.plots
print.trellis.plots <- function(plt.list=NULL,cols=NA,rows=NA,
                                x.size=c(0.03,0.99),y.size=c(0.03,0.99),x.overlap=0.02,y.overlap=0.02,
                                use.plt.void=FALSE,plt.void=NULL,more=FALSE,trace=FALSE){
  # Purpose: Help place single Trellis plots on a single page
  # Created: April 15, 2003
  # Revised: April 15, 2003
  # Revised: November 20, 2011 for R (print.trellis = print)
  # Name   : Claus E. Andersen
  # Library: clan
  # plt.list  = list of trellis plots. NULL is ok.
  # cols      = specify a given number of columns
  # rows      = specify a given number of rows
  # x.size    = The x-region to be used (0-1)
  # y.size    = The y-region to be used (0-1)
  # x.overlap = The amount of x-overlap
  # y.overlap = The amount of y-overlap
  # use.plt.void = Control if some plot should be made where elements in the plt.list are NULL
  # plt.void     = A default void plot may be supplied
  # trace        = control text output
  # more         = controls if the last plot in this call is the final one (see print.trellis)
  # Special features: If one plot is NULL, that position on the page will be left empty
  # If all plots are NULL, then an empty page will be printed (so subsequent output can safely be dumped on that page).
  # Sample calls:
  # plt0 <- xyplot(rnorm(10)~rnorm(10))
  # print.trellis.plots(list(plt0,plt0,plt0),3,1)
  # print.trellis.plots(list(plt0,plt0,NULL,plt0,plt0,plt0,plt0,plt0,plt0,plt0,plt0,plt0))
  # print.trellis.plots(list(plt0,plt0,NULL,plt0,plt0,plt0,plt0,plt0,plt0,plt0,plt0,plt0),x.overlap=0.04,y.overlap=.04)
  # print.trellis.plots(list(NULL,NULL,NULL),plt.void=xyplot(0~1,type="n",scales=list(draw=F),ylab="",main="\n\n\n\nVOID"),use.plt.void=T)
  # print.trellis.plots(list(NULL,plt0,plt0),plt.void=xyplot(0~1,type="n",scales=list(draw=F),ylab="",main="\n\n\n\nVOID"),use.plt.void=T)
  
  # Observe, that if the plots should not have the same siza, then you simply use
  # the more=T option and call print.trellis.plots more than once.
  # For example:
  # print.trellis.plots(list(plt0,NULL,plt0,NULL),more=T)
  # print.trellis.plots(list(NULL,plt0))
  
  # Plot, to show in case of no data (may be supplied in call):
  if(is.null(plt.void)){plt.void <- xyplot(c(0,1,1,0,0)~c(0,0,1,1,0),,type="l",scales=list(draw=F),nobox=T,xlab="",ylab="")}
  plt.nothing <- xyplot(c(0,1,1,0,0)~c(0,0,1,1,0),,type="n",scales=list(draw=F),nobox=T,xlab="",ylab="")
  
  # Calculate the total number of plots:
  N <- length(plt.list)
  
  # Calculate the number of columns and rows (if not already specified):
  if(is.na(cols)) cols <- ceiling(N^0.5)
  if(is.na(rows)) rows <- ceiling(N/cols)
  
  # Split the x (and y) axes evenly into sections:
  dx <- (x.size[2]-x.size[1])/cols
  dy <- (y.size[2]-y.size[1])/rows
  xp <- seq(x.size[1],x.size[2],length=cols+1)
  yp <- seq(y.size[1],y.size[2],length=rows+1)
  
  # Define the extension of each column:
  xpp <- rep(NA,cols*2)
  for(i in 1:cols){
    xpp[2*i-1] <- xp[i]-x.overlap
    xpp[2*i]   <- xp[i+1]+x.overlap
  }
  
  # Define the extension of each row:
  ypp <- rep(NA,rows*2)
  for(i in 1:rows){
    ypp[2*i-1] <- yp[i]-y.overlap
    ypp[2*i]   <- yp[i+1]+y.overlap
  }
  
  more.plots <- rep(T,N)
  last.plot <- 1
  for(i in 1:N) if(!is.null(plt.list[[i]])) {last.plot <- i}
  
  if(use.plt.void) last.plot <- N
  more.plots[last.plot] <- more # from the function call
  
  # Now treat each plot in the list
  printed.something <- F
  col0 <- 0
  row0 <- 1
  
  for(i in 1:N) {
    col0 <- col0 + 1
    if(col0>cols) {
      col0 <- 1
      row0 <- row0 +1
    }
    if(trace){
      print(paste(col0,row0))
      print(c(xpp[2*col0-1],ypp[2*row0-1], xpp[2*col0],  ypp[2*row0]))
    } # if trace
    
    if(!is.null(plt.list[[i]])){
      print(plt.list[[i]],position=c(xpp[2*col0-1],ypp[2*row0-1], xpp[2*col0],  ypp[2*row0]),more=more.plots[i])
      printed.something <- T
    } else  {
      if(use.plt.void){
        print(plt.void,position=c(xpp[2*col0-1],ypp[2*row0-1], xpp[2*col0],  ypp[2*row0]),more=more.plots[i])
        printed.something <- T
      }
    }
    
    #if(!is.null(plt.list[[i]])){
    #  print.trellis(plt.list[[i]],position=c(xpp[2*col0-1],ypp[2*row0-1], xpp[2*col0],  ypp[2*row0]),more=more.plots[i])
    #  printed.something <- T
    #} # if not null plt
    
    
    
  } # for loop i in 1:N
  
  if(!printed.something){
    plot(plt.nothing,more=more)
  }
  
  return(paste("Plotted",N,"graph(s)"))
} # end print.trellis.plots

