
#' @title Robust prepanel function for lattice plots (xlim).
#' @description 
#' Lattice has a problem if there is only one single value and if
#' that value is very large (for example, y=6e66). The following
#' prepanel functions can be used to handle such situations.
#' See also \link(prepanel.robust.y) and \link(prepanel.robust.xy) 
#' @usage none
#' @name prepanel.robust.x
#' @author Claus E. Andersen
#' @return a list with xlim-values for a trellis plot.
#' @param x gives the x-values.
#' @param y gives the y-values.
#' @param ... for additional parameters.
#' @export prepanel.robust.x
prepanel.robust.x <- function(x,y,...){
  xlim <- range(x)
  if(length(x)<1.1){xlim <- x*c(0.99,1.01)}
  list(xlim=xlim)
}# prepanel.robust.x


#' @title Robust prepanel function for lattice plots (ylim).
#' @description 
#' Lattice has a problem if there is only one single value and if
#' that value is very large (for example, y=6e66). The following
#' prepanel functions can be used to handle such situations.
#' See also \link(prepanel.robust.x) and \link(prepanel.robust.xy) 
#' @usage none
#' @name prepanel.robust.y
#' @author Claus E. Andersen
#' @return a list with ylim-values for a trellis plot.
#' @param x gives the x-values.
#' @param y gives the y-values.
#' @param ... for additional parameters.
#' @export prepanel.robust.y
prepanel.robust.y <- function(x,y,...){
  ylim <- range(y)
  if(length(y)<1.1){ylim <- y*c(0.99,1.01)}
  list(ylim=ylim)
}# prepanel.robust.y



#' @title Robust prepanel function for lattice plots (xlim and ylim).
#' @description 
#' Lattice has a problem if there is only one single value and if
#' that value is very large (for example, y=6e66). The following
#' prepanel functions can be used to handle such situations.
#' See also \link(prepanel.robust.x) and \link(prepanel.robust.y) 
#' @usage none
#' @name prepanel.robust.xy
#' @author Claus E. Andersen
#' @return a list with ylim-values for a trellis plot.
#' @param x gives the x-values.
#' @param y gives the y-values.
#' @param ... for additional parameters.
#' @export prepanel.robust.xy
prepanel.robust.xy <- function(x,y,...){
  xlim <- range(x)
  if(length(x)<1.1){xlim <- x*c(0.99,1.01)}
  ylim <- range(y)
  if(length(y)<1.1){ylim <- y*c(0.99,1.01)}
  list(xlim=xlim,ylim=ylim)
} # prepanel.robust.xy
