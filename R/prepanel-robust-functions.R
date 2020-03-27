#' @title Robust prepanel function for lattice plots (xlim).
#' @description 
#' This prepanel function can provide useful scales even if there is
#' only one data point or if all values are identical (no variability).
#' A limit parameter equal to 0.01 means that the scale will at least
#' go from 1-limit = 0.99 to 1+limit = 1.01 times the reference value.
#' The parameter limit.robust is for the situation when there is only one data point
#' that is not NA. The parameter limit.mean uses the mean as reference, and 
#' limit.median uses the median value as reference.
#' Note that the lattice plots generally fail (without a prepanel function) if there is only 
#' one single point and if that value is very large (for example, y=6e66). 
#' The following prepanel functions can be used to handle such situations.
#' The prepanel function can also be used to ascertain that a useful
#' minimum scale of say +/- 0.5\% is always shown. Note that the actual
#' scale may be larger (i.e. the selected scale limits will always include the
#' data).
#' See also \link{prepanel.robust.y} and \link{prepanel.robust.xy}
#' Example:
#' 
#' N <- 40 # two of which are NA-values
#' 
#' df <- data.frame(model= sample(c("M1"),size=N,replace=TRUE), cap= sample(c("A","B","C"),size=N,replace=TRUE), 
#' pA = c(NA,NA,rnorm(N-2,mean=9e99,sd=0.001)))
#' 
#' dotplot(cap ~ pA | paste(model),
#'  
#' limit.mean = 0.05, # Make sure we have an x-scale of at least +/- 5\% of the mean
#' 
#' prepanel=prepanel.robust.x,
#' 
#' groups=cap, 
#' 
#' auto.key=list(TRUE),
#' 
#' data=df,
#' 
#' panel=function(x,y,...){
#' 
#' panel.dotplot(x,y,...)
#' 
#' })
#' @usage none
#' @name prepanel.robust.x
#' @author Claus E. Andersen
#' @return a list with xlim-values for a trellis plot.
#' @param x gives the x-values.
#' @param y gives the y-values.
#' @param ... for additional parameters.
#' @param limit.robust limit to use is there is only one data point.
#' @param limit.mean is the mean-based minimum limit.
#' @param limit.median is the median-based minimum limit. 
#' @export prepanel.robust.x
prepanel.robust.x <- function(x,y,...,limit.robust=0.01, limit.mean=0.0001, limit.median=NULL){
  xlim <- range(x,na.rm=TRUE)
  # Only one point:
  if(sum(!is.na(x))<1.1){xlim <- mean(x,na.rm=TRUE)*c(1-limit.robust,1+limit.robust)}
  if(!is.null(limit.mean)){xlim <- range(c(xlim,mean(x,na.rm=TRUE)*c(1-limit.mean,1+limit.mean)))}
  if(!is.null(limit.median)){xlim <- range(c(xlim,median(x,na.rm=TRUE)*c(1-limit.median,1+limit.median)))}
  list(xlim=xlim)
}# prepanel.robust.x




#' @title Robust prepanel function for lattice plots (ylim).
#' @description 
#' This prepanel function can provide useful scales even if there is
#' only one data point or if all values are identical (no variability).
#' A limit parameter equal to 0.01 means that the scale will at least
#' go from 1-limit = 0.99 to 1+limit = 1.01 times the reference value.
#' The parameter limit.robust is for the situation when there is only one data point
#' that is not NA. The parameter limit.mean uses the mean as reference, and 
#' limit.median uses the median value as reference.
#' Note that the lattice plots generally fail (without a prepanel function) if there is only 
#' one single point and if that value is very large (for example, y=6e66). 
#' The following prepanel functions can be used to handle such situations.
#' The prepanel function can also be used to ascertain that a useful
#' minimum scale of say +/- 0.5\% is always shown. Note that the actual
#' scale may be larger (i.e. the selected scale limits will always include the
#' data).
#' See also \link{prepanel.robust.x} and \link{prepanel.robust.xy}
#' Example (using the xlim version):
#' 
#' N <- 40 # two of which are NA-values
#' 
#' df <- data.frame(model= sample(c("M1"),size=N,replace=TRUE), cap= sample(c("A","B","C"),size=N,replace=TRUE), 
#' pA = c(NA,NA,rnorm(N-2,mean=9e99,sd=0.001)))
#' 
#' dotplot(cap ~ pA | paste(model),
#'  
#' limit.mean = 0.05, # Make sure we have an x-scale of at least +/- 5\% of the mean
#' 
#' prepanel=prepanel.robust.x,
#' 
#' groups=cap, 
#' 
#' auto.key=list(TRUE),
#' 
#' data=df,
#' 
#' panel=function(x,y,...){
#' 
#' panel.dotplot(x,y,...)
#' 
#' })
#' @usage none
#' @name prepanel.robust.y
#' @author Claus E. Andersen
#' @return a list with ylim-values for a trellis plot.
#' @param x gives the x-values.
#' @param y gives the y-values.
#' @param ... for additional parameters.
#' @param limit.robust limit to use is there is only one data point.
#' @param limit.mean is the mean-based minimum limit.
#' @param limit.median is the median-based minimum limit. 
#' @export prepanel.robust.y
prepanel.robust.y <- function(x,y,...,limit.robust=0.01, limit.mean=0.0001, limit.median=NULL){
  ylim <- range(y,na.rm=TRUE)
  # Only one point:
  if(sum(!is.na(y))<1.1){ylim <- mean(y,na.rm=TRUE)*c(1-limit.robust,1+limit.robust)}
  if(!is.null(limit.mean)){ylim <- range(c(ylim,mean(y,na.rm=TRUE)*c(1-limit.mean,1+limit.mean)))}
  if(!is.null(limit.median)){ylim <- range(c(ylim,median(y,na.rm=TRUE)*c(1-limit.median,1+limit.median)))}
  list(ylim=ylim)
}# prepanel.robust.y



#' @title Robust prepanel function for lattice plots (ylim).
#' @description 
#' This prepanel function can provide useful scales even if there is
#' only one data point or if all values are identical (no variability).
#' A limit parameter equal to 0.01 means that the scale will at least
#' go from 1-limit = 0.99 to 1+limit = 1.01 times the reference value.
#' The parameter limit.robust is for the situation when there is only one data point
#' that is not NA. The parameter limit.mean uses the mean as reference, and 
#' limit.median uses the median value as reference.
#' Note that the lattice plots generally fail (without a prepanel function) if there is only 
#' one single point and if that value is very large (for example, y=6e66). 
#' The following prepanel functions can be used to handle such situations.
#' The prepanel function can also be used to ascertain that a useful
#' minimum scale of say +/- 0.5\% is always shown. Note that the actual
#' scale may be larger (i.e. the selected scale limits will always include the
#' data).
#' See also \link{prepanel.robust.x} and \link{prepanel.robust.xy}
#' Example (using the xlim version):
#' 
#' N <- 40 # two of which are NA-values
#' 
#' df <- data.frame(model= sample(c("M1"),size=N,replace=TRUE), cap= sample(c("A","B","C"),size=N,replace=TRUE), 
#' pA = c(NA,NA,rnorm(N-2,mean=9e99,sd=0.001)))
#' 
#' dotplot(cap ~ pA | paste(model),
#'  
#' limit.mean = 0.05, # Make sure we have an x-scale of at least +/- 5\% of the mean
#' 
#' prepanel=prepanel.robust.x,
#' 
#' groups=cap, 
#' 
#' auto.key=list(TRUE),
#' 
#' data=df,
#' 
#' panel=function(x,y,...){
#' 
#' panel.dotplot(x,y,...)
#' 
#' })
#' @usage none
#' @name prepanel.robust.xy
#' @author Claus E. Andersen
#' @return a list with xlim-values and ylim-values for a trellis plot.
#' @param x gives the x-values.
#' @param y gives the y-values.
#' @param ... for additional parameters.
#' @param limit.robust limit to use is there is only one data point.
#' @param limit.mean is the mean-based minimum limit.
#' @param limit.median is the median-based minimum limit. 
#' @export prepanel.robust.xy
prepanel.robust.xy <- function(x,y,...,limit.robust=0.01, limit.mean=0.0001, limit.median=NULL){
  
  xlim <- range(x,na.rm=TRUE)
  # Only one point:
  if(sum(!is.na(x))<1.1){xlim <- mean(x,na.rm=TRUE)*c(1-limit.robust,1+limit.robust)}
  if(!is.null(limit.mean)){xlim <- range(c(xlim,mean(x,na.rm=TRUE)*c(1-limit.mean,1+limit.mean)))}
  if(!is.null(limit.median)){xlim <- range(c(xlim,median(x,na.rm=TRUE)*c(1-limit.median,1+limit.median)))}  

    ylim <- range(y,na.rm=TRUE)
  # Only one point:
  if(sum(!is.na(y))<1.1){ylim <- mean(y,na.rm=TRUE)*c(1-limit.robust,1+limit.robust)}
  if(!is.null(limit.mean)){ylim <- range(c(ylim,mean(y,na.rm=TRUE)*c(1-limit.mean,1+limit.mean)))}
  if(!is.null(limit.median)){ylim <- range(c(ylim,median(y,na.rm=TRUE)*c(1-limit.median,1+limit.median)))}

  list(xlim=xlim,ylim=ylim)
}# prepanel.robust.xy
