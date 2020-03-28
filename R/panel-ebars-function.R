#' @title Errorbar panel for Lattice graphics
#' @description This function has been designed for plotting of errorbars in lattice plots.
#' It handles both x and y-errorbars as well as asymmetrical and symmetrical errorbars.
#' Sevarel features such as the whisker widths and other praghical parameters can
#' be controlled. Furthermore, the error bars can be offset from the main data 
#' point, and the inner part of the bars can be set not to be drawn (such that 
#' the main data point stands out). The function can also plot a single point 
#' without use of panel.xyplot or panel.superpose.
#'
#' Provide either x.minus or x.plus or use x.err if the errorbar is 
#' symmetrical. Likewise for the y-errorbar. Apply NA values for errorbars
#' that should not be plotted (e.g. if they are within symbols).
#'    
#' @usage
#' See the function panel.ebars.demo
#' @name panel.ebars
#' @author Claus E. Andersen
#' @return A Lattice panel 
#' @export panel.ebars
#' @param x = x-coordinate for the data points.
#' @param y = y-coordinate for the data points.
#' @param x.wanted = TRUE means that the x-errorbars will be plotted.
#' @param y.wanted = TRUE means that the y-errorbars will be plotted.
#' @param x.err = the size of symmetrical x-errorbars (i.e. x.err = x.minus = x.plus).
#'            Note, that x.err, x.minus, or x.plus must be of the same length as 
#'            x and y. This is so because these vectors are manipulated deeper down 
#'            using the subscripts argument. Also, note that it is not enough to just
#'            set x.err etc. equal to a named column in the main dataframe. So, if the
#'            main dataframe is called df, and if it contains a column called u.Gy that 
#'            you want to use as errorbar, then you must write x.err = df$u.Gy (it is
#'            NOT enough to write x.err = u.Gy even though data=df). Likewise, if you
#'            want to set the errorbar to a constant value, you must use
#'            something like x.err = rep(2.3,nrow(df)) rather than just x.err = 2.3.  
#' @param x.minus = the size of the lower x-errorbars (if assymetrical)
#' @param x.plus  = the size of the upper x-errorbars (if assymetrical)
#' @param x.width = the width (in mm) of the whiskers on the x-errorbars.
#' @param x.inner = the free space (in mm) around the main data point where no part of the errorbas should be drawn.
#' @param x.offset = the offset (in mm) of the x-errorbar.
#' @param gp.x = graphical parameters for the x-errorbars.
#' @param gp.xwhisker = graphical parameters for the whiskers for the x-errorbars.
#' @param point.wanted = TRUE means that a separate point will be drawn at the central data.point.
#' @param gp.point = graphical parameters for the separate data point to be drawn.  
panel.ebars <- function(x, y, subscripts,
           x.wanted = TRUE, 
           y.wanted = TRUE, 
           x.err = NULL, x.minus = NULL, x.plus = NULL, 
           y.err = NULL, y.minus = NULL, y.plus = NULL, 
           x.width  = 2, y.width  = 2,
           x.inner  = 2, y.inner  = 2,
           x.offset = 0, y.offset = 0, 
           gp.x=gpar(),gp.xwhisker=gpar(),
           gp.y=gpar(),gp.ywhisker=gpar(),
           point.wanted = FALSE, 
           gp.point=gpar(),
           ...){
    # This function has been designed for plotting of errorbars in lattice plots.
    # It handles both x and y-errorbars as well as asymmetrical and symmetrical errorbars.
    # Sevarel features such as the whisker widths and other praghical parameters can
    # be controlled. Furthermore, the error bars can be offset from the main data 
    # point, and the inner part of the bars can be set not to be drawn (such that 
    # the main data point stands out). The function can also plot a single point 
    # without use of panel.xyplot or panel.superpose.    
    
    # This function was originally created for the clan library (Dec. 30, 2003).
    # Revised: May 17, 2012 (for R)
    # Revised: May 19, 2012
    # Revised: May 20, 2012
    # Revised: May 22, 2012
    # Name   : Claus E. Andersen
    #
    # Parameters:
    #    x = x-coordinate for the data points.
    #    y = y-coordinate for the data points.
    #    x.wanted = TRUE means that the x-errorbars will be plotted.
    #    y.wanted = TRUE means that the y-errorbars will be plotted.
    #    x.err = the size of symmetrical x-errorbars (i.e. x.err = x.minus = x.plus).
    #            Note, that x.err, x.minus, or x.plus must be of the same length as 
    #            x and y. This is so because these vectors are manipulated deeper down 
    #            using the subscripts argument. Also, note that it is not enough to just
    #            set x.err etc. equal to a named column in the main dataframe. So, if the
    #            main dataframe is called df, and if it contains a column called u.Gy that 
    #            you want to use as errorbar, then you must write x.err = df$u.Gy (it is
    #            NOT enough to write x.err = u.Gy even though data=df). Likewise, if you
    #            want to set the errorbar to a constant value, you must use
    #            something like x.err = rep(2.3,nrow(df)) rather than just x.err = 2.3.  
    #    x.minus = the size of the lower x-errorbars (if assymetrical)
    #    x.plus  = the size of the upper x-errorbars (if assymetrical)
    #    x.width = the width (in mm) of the whiskers on the x-errorbars.
    #    x.inner = the free space (in mm) around the main data point where no part of the errorbas should be drawn.
    #    x.offset = the offset (in mm) of the x-errorbar.
    #    gp.x = graphical parameters for the x-errorbars.
    #    gp.xwhisker = graphical parameters for the whiskers for the x-errorbars.
    #    point.wanted = TRUE means that a separate point will be drawn at the central data.point.
    #    gp.point = graphical parameters for the separate data point to be drawn.  
    #
    # Notes:
    #    Provide either x.minus or x.plus or use x.err if the errorbar is 
    #    symmetrical. Likewise for the y-errorbar. Apply NA values for errorbars
    #    that should not be plotted (e.g. if they are within symbols).
    #
    # Sample call : see panel.ebars.demo()
    # ###################################################
    # If no plus or minus are defined then use x.err
    if(is.null(x.err))   x.err   <- rep(NA,length(x))
    if(is.null(x.plus))  x.plus  <- x.err
    if(is.null(x.minus)) x.minus <- x.err
    
    # If no plus or minus are defined then use y.err
    if(is.null(y.err))   y.err   <- rep(NA,length(x))
    if(is.null(y.plus))  y.plus  <- y.err
    if(is.null(y.minus)) y.minus <- y.err
    
    # x-direction errorbar
    if(x.wanted) { 
      x.plus <- x + x.plus[subscripts]
      x.minus <- x - x.minus[subscripts]
      x.inner <- convertX(unit(c(0,x.inner),"mm"),"native",valueOnly=TRUE)
      x.inner <- x.inner[2] - x.inner[1]  
      x.offset <- convertY(unit(c(0,x.offset),"mm"),"native",valueOnly=TRUE)
      x.offset <- x.offset[2] - x.offset[1]   
      y0 <- y + x.offset
      grid.segments(x.minus, y0, pmax(x.minus,x-x.inner),y0, gp=gp.x, default.units='native')
      grid.segments(pmin(x.plus,x+x.inner), y0, x.plus,  y0, gp=gp.x, default.units='native')
      if(x.width>0){
        # Convert whisker size in mm to (native) user coordinates
        dy.usr <- convertY(unit(c(0,x.width),"mm"),"native",valueOnly=TRUE)
        dy.usr <- dy.usr[2] - dy.usr[1]   
        grid.segments(x.plus,  y0 - dy.usr/2, x.plus,  y0 + dy.usr/2, gp=gp.xwhisker,default.units='native')
        grid.segments(x.minus, y0 - dy.usr/2, x.minus, y0 + dy.usr/2, gp=gp.xwhisker,default.units='native')
      } # x.width > 0
    } #end if x.wanted
    
    # y-direction errorbar
    if(y.wanted) {
      y.plus  <- y + y.plus[subscripts]
      y.minus <- y - y.minus[subscripts]
      y.inner <- convertY(unit(c(0,y.inner),"mm"),"native",valueOnly=TRUE)
      y.inner <- y.inner[2] - y.inner[1]  
      y.offset <- convertX(unit(c(0,y.offset),"mm"),"native",valueOnly=TRUE)
      y.offset <- y.offset[2] - y.offset[1]   
      x0 <- x + y.offset
      grid.segments(x0, y.minus, x0, pmax(y.minus,y-y.inner),gp=gp.y,default.units='native')
      grid.segments(x0, pmin(y.plus,y+y.inner), x0, y.plus  ,gp=gp.y,default.units='native')
      if(y.width>0){
        # Convert whisker size in mm to (native) user coordinates
        dx.usr <- convertX(unit(c(0,y.width),"mm"),"native",valueOnly=TRUE)
        dx.usr <- dx.usr[2] - dx.usr[1]  
        grid.segments(x0 - dx.usr/2, y.plus,  x0 + dx.usr/2, y.plus ,gp=gp.ywhisker, default.units='native')
        grid.segments(x0 - dx.usr/2, y.minus, x0 + dx.usr/2, y.minus,gp=gp.ywhisker, default.units='native')
      } # y.width > 0 
    } # y.wanted
    
    if(point.wanted){
      grid.points(x,y,gp=gp.point,default.units='native',...)
    }
    
  } # end panel.ebars


#' @title Improved errorbar panel for Lattice graphics (version 2)
#' @description This function has been designed for plotting of errorbars in lattice plots.
#' It handles both x and y-errorbars as well as asymmetrical and symmetrical errorbars.
#' Sevarel features such as the whisker widths and other praghical parameters can
#' be controlled. Furthermore, the error bars can be offset from the main data 
#' point, and the inner part of the bars can be set not to be drawn (such that 
#' the main data point stands out). The function can also plot a single point 
#' without use of panel.xyplot or panel.superpose.
#'
#' Provide either x.minus or x.plus or use x.err if the errorbar is 
#' symmetrical. Likewise for the y-errorbar. Apply NA values for errorbars
#' that should not be plotted (e.g. if they are within symbols).
#'    
#' Improvements: Now works with lattice dotplots.
#'   
#' @usage
#' See the function panel.ebars.demo
#' @name panel.ebars2
#' @author Claus E. Andersen
#' @return A Lattice panel 
#' @param x = x-coordinate for the data points.
#' @param y = y-coordinate for the data points.
#' @param x.wanted = TRUE means that the x-errorbars will be plotted.
#' @param y.wanted = TRUE means that the y-errorbars will be plotted.
#' @param x.err = the size of symmetrical x-errorbars (i.e. x.err = x.minus = x.plus).
#'            Note, that x.err, x.minus, or x.plus must be of the same length as 
#'            x and y. This is so because these vectors are manipulated deeper down 
#'            using the subscripts argument. Also, note that it is not enough to just
#'            set x.err etc. equal to a named column in the main dataframe. So, if the
#'            main dataframe is called df, and if it contains a column called u.Gy that 
#'            you want to use as errorbar, then you must write x.err = df$u.Gy (it is
#'            NOT enough to write x.err = u.Gy even though data=df). Likewise, if you
#'            want to set the errorbar to a constant value, you must use
#'            something like x.err = rep(2.3,nrow(df)) rather than just x.err = 2.3.  
#' @param x.minus = the size of the lower x-errorbars (if assymetrical)
#' @param x.plus  = the size of the upper x-errorbars (if assymetrical)
#' @param x.width = the width (in mm) of the whiskers on the x-errorbars.
#' @param x.inner = the free space (in mm) around the main data point where no part of the errorbas should be drawn.
#' @param x.offset = the offset (in mm) of the x-errorbar.
#' @param gp.x = graphical parameters for the x-errorbars.
#' @param gp.xwhisker = graphical parameters for the whiskers for the x-errorbars.
#' @param point.wanted = TRUE means that a separate point will be drawn at the central data.point.
#' @param gp.point = graphical parameters for the separate data point to be drawn.  
#' @export panel.ebars2
panel.ebars2 <- function (x, y, subscripts, x.wanted = TRUE, y.wanted = TRUE, 
            x.err = NULL, x.minus = NULL, x.plus = NULL, y.err = NULL, 
            y.minus = NULL, y.plus = NULL, x.width = 4, y.width = 4, 
            x.inner = 3, y.inner = 2, x.offset = 0, y.offset = 0, gp.x = gpar(),  
            gp.xwhisker = gpar(), gp.y = gpar(), gp.ywhisker = gpar(),
            point.wanted = FALSE, gp.point = gpar(), ...) 
  {
    if (is.null(x.err)) 
      x.err <- rep(NA, length(x))
    if (is.null(x.plus)) 
      x.plus <- x.err
    if (is.null(x.minus)) 
      x.minus <- x.err
    if (is.null(y.err)) 
      y.err <- rep(NA, length(x))
    if (is.null(y.plus)) 
      y.plus <- y.err
    if (is.null(y.minus)) 
      y.minus <- y.err
    if (x.wanted) {
      x.plus <- x + x.plus[subscripts]
      x.minus <- x - x.minus[subscripts]
      x.inner <- convertX(unit(c(0, x.inner), "mm"), "native", 
                          valueOnly = TRUE)
      x.inner <- x.inner[2] - x.inner[1]
      x.offset <- convertY(unit(c(0, x.offset), "mm"), "native", 
                           valueOnly = TRUE)
      x.offset <- x.offset[2] - x.offset[1]
      y0 <- y + x.offset
      grid.segments(x.minus, y0, pmax(x.minus, x - x.inner), 
                    y0, gp = gp.x, default.units = "native")
      grid.segments(pmin(x.plus, x + x.inner), y0, x.plus, 
                    y0, gp = gp.x, default.units = "native")
      if (x.width > 0) {
        dy.usr <- convertY(unit(c(0, x.width), "mm"), "native", 
                           valueOnly = TRUE)
        dy.usr <- dy.usr[2] - dy.usr[1]
        grid.segments(x.plus, y0 - dy.usr/2, x.plus, y0 + 
                        dy.usr/2, gp = gp.xwhisker, default.units = "native")
        grid.segments(x.minus, y0 - dy.usr/2, x.minus, y0 + 
                        dy.usr/2, gp = gp.xwhisker, default.units = "native")
      }
    }
    if (y.wanted) {
      y.plus <- y + y.plus[subscripts]
      y.minus <- y - y.minus[subscripts]
      y.inner <- convertY(unit(c(0, y.inner), "mm"), "native", 
                          valueOnly = TRUE)
      y.inner <- y.inner[2] - y.inner[1]
      y.offset <- convertX(unit(c(0, y.offset), "mm"), "native", 
                           valueOnly = TRUE)
      y.offset <- y.offset[2] - y.offset[1]
      x0 <- x + y.offset
      grid.segments(x0, y.minus, x0, pmax(y.minus, y - y.inner), 
                    gp = gp.y, default.units = "native")
      grid.segments(x0, pmin(y.plus, y + y.inner), x0, y.plus, 
                    gp = gp.y, default.units = "native")
      if (y.width > 0) {
        dx.usr <- convertX(unit(c(0, y.width), "mm"), "native", 
                           valueOnly = TRUE)
        dx.usr <- dx.usr[2] - dx.usr[1]
        grid.segments(x0 - dx.usr/2, y.plus, x0 + dx.usr/2, 
                      y.plus, gp = gp.ywhisker, default.units = "native")
        grid.segments(x0 - dx.usr/2, y.minus, x0 + dx.usr/2, 
                      y.minus, gp = gp.ywhisker, default.units = "native")
      }
    }
    if (point.wanted) {
      grid.points(x, y, gp = gp.point, default.units = "native")  
      #  I removed the dots to get rid on an "unused arguments error!!  ...)
      # Feb. 22, 2019
    }
  }# panel.ebars2

