#' @title Lattice plotting panel for binned data (e.g. aggregared standard deviations)
#' @description This is a panel function for Lattice plotting. The data is binned in accordance with the 
#' x-values and calculate standard deviations, for example. 
#' See also \link{panel.binned.errorbars}, \link{panel.ebars} and \link{panel.ebars2}.

#' @usage
#' see the body of the function plut the function called \link{panel.binned.errorbars.demo}.
#' @name panel.binned.errorbars
#' @author Claus E. Andersen
#' @return plt A Lattice plot showing the colors
#' @param x is the x-coordinates for the data
#' @param y is the y-coordinates od the data
#' @export panel.binned.errorbars
panel.binned.errorbars <- function(x, y, subscripts, 
           groups, 
           type = "p", 
           lwd = superpose.line$lwd, 
           lty = superpose.line$lty, 
           pch = superpose.symbol$pch, 
           cex = superpose.symbol$cex, 
           font = superpose.symbol$font, 
           col = NULL, 
           err.control = list(pool.groups = FALSE, x.offset = 0, point.radius = 0, 
                              show.center = TRUE, type ="p", 
                              show = TRUE, x.width = 2, lwd.bar = 2, col.bar = 1, lwd.line = 2, 
                              col.line = 1, lty.line = 1, 
                              show2 = FALSE, x.width2 = 4, lwd.bar2 = 2, col.bar2 = 1, lwd.line2 = 2, 
                              col.line2 = 1, lty.line2 = 1, col.nogroup = 1, pch.nogroup = 16, 
                              cex.nogroup = 1, lwd.nogroup = 1,
                              x.bins.function = function(x){round.resolution(x, 1, 0)}, 
                              x.center.function = function(x){mean(x, na.rm = TRUE)},
                              y.center.function = function(x){mean(x, na.rm = TRUE)}, 
                              y.low.function = function(x){ok <- !is.na(x)
                                                           mean(x[ok]) - (var(x[ok])/sum(ok))^0.5
                                                          }, 
                              y.high.function = function(x){ok <- !is.na(x)
                                                            mean(x[ok]) + (var(x[ok])/sum(ok))^0.5
                                                           }, 
                              y.low2.function = function(x){ok <- !is.na(x)
                                                            mean(x[ok]) - var(x[ok])^0.5
                                                           }, 
                              y.high2.function = function(x){ok <- !is.na(x)
                                                            mean(x[ok]) + var(x[ok])^0.5
                                                            }, 
                              y.lab.function = function(x){ok <- !is.na(x)
                                                           mean(x[ok])
                                                          }, 
                              x.offset.lab = 0, 
                              y.offset.lab.N = 0, 
                              y.offset.lab.center = -3, 
                              srt.lab = 0, 
                              cex.lab = 1, 
                              adj.lab = 0, 
                              col.lab = 1, 
                              round.lab = 1, 
                              show.lab.N = FALSE,
                              show.lab.center = FALSE, 
                              transform.to.residuals=FALSE), 
                              trace = FALSE, ...)
{
    if(trace)
      wrline("panel.binned.errorbars (begin)")
    # Purpose: Bin the data in accordance with the x-values and calculate standard deviations, for example.
    #          Plot the results and add text.
    # Created: October 16, 2005
    # Revised: October 18, 2005
    # Revised: October 31, 2005
    # Revised: November  23, 2005: (now ensures that groups is a factor. Also added a trace-argument)
    # Revised: December  7, 2005 (use codes.ca to ensure same coding of factor and ordered objects)
    # Revised: December 12, 2005 (now lwd of points can be controlled from set.trellis()).
    # Revised: January 29, 2006 (lwd.nogroup default has been fixed - otherwise problems w. pool.groups).
    # Revised: January 29, 2006 (added the transform.to.residuals options).
    # Revised: January 30, 2006 (added "nothing" to list of legal transforms).
    # Revised: November 12, 2011 Moved from S-plus to R.
    
    # FOR EXAMPLES: SEE panel.binned.errorbars.demo()
    # Name:    Claus E. Andersen
    # We have to test if the arguments are truely in the supplied list. This solves the
    # problem of partial matching. The use may provide only show2 (and NOT show), but in the test (see below) is.null(err.control$show))
    # returns F. The names.given lists all the names included in the err.control list.
    
    # What is the use of the "transform.to.residuals"?
    # Consider an OSL growth curve with measurements at 0, 1, 2, and 5 Gy. At each
    # dose-point we have several measurements at different stimulation temperatures
    # 10, 20, 30, and 40 decC. We make a plot of the OSL results, and we observe that
    # at each dose-point there may be some significant difference among the
    # results obtained at different temperatures. We could now proceed along two
    # lines of analysis: (1) We could fit a model to obtain the "true" growth
    # curves, and we could subsequently investigate the residuals around
    # these models. (2) We could simply investigate the difference from temperature
    # to temperature (i.e. from group to group) using the overall group mean
    # as reference. For example, consider all the 2 Gy-data. First, we calculate the overall
    # 2-Gy mean (i.e.calculate the pooled mean regardless of temperature). Then
    # subtract this value from the temperature specific means for the 2-Gy data.
    # The parameter "transform.to.residuals" takes the following values (m=mean for given group, and mg=pooled group mean):
    # F: No transformation
    # T or "abs": m - mg
    # "norm"    : m/mg
    # "rel"     : (m-mg)/mg
    # "pct"     : (m-mg)/mg * 100
    
    names.given <- names(err.control)
    if(is.null(err.control$pool.groups))
      err.control$pool.groups <- F
    if(is.null(err.control$x.offset))
      err.control$x.offset <- 0
    if(is.null(err.control$show.center))
      err.control$show.center <- T
    if(is.null(err.control$type))
      err.control$type <- "p"
    # Defaults for the inner error bars
    if(!is.element("show", names.given) | is.null(err.control$show)) err.control$show <- T
    if(!is.element("point.radius", names.given) | is.null(err.control$point.radius))
      err.control$point.radius <- 0
    if(!is.element("x.width", names.given) | is.null(err.control$x.width))
      err.control$x.width <- 2
    if(!is.element("lwd.bar", names.given) | is.null(err.control$lwd.bar))
      err.control$lwd.bar <- 2
    if(!is.element("col.bar", names.given) | is.null(err.control$col.bar))
      err.control$col.bar <- 1
    if(!is.element("lwd.line", names.given) | is.null(err.control$lwd.line))
      err.control$lwd.line <- 2
    if(!is.element("col.line", names.given) | is.null(err.control$col.line))
      err.control$col.line <- 1
    if(!is.element("lty.line", names.given) | is.null(err.control$lty.line))
      err.control$lty.line <- 1
    # Defaults for the outer error bars
    if(is.null(err.control$show2)) err.control$show2 <- F
    if(is.null(err.control$point.radius2))
      err.control$point.radius2 <- 0
    if(is.null(err.control$x.width2))
      err.control$x.width2 <- 2
    if(is.null(err.control$lwd.bar2))
      err.control$lwd.bar2 <- 2
    if(is.null(err.control$col.bar2))
      err.control$col.bar2 <- 1
    if(is.null(err.control$lwd.line2))
      err.control$lwd.line2 <- 2
    if(is.null(err.control$col.line2))
      err.control$col.line2 <- 1
    if(is.null(err.control$lty.line2))
      err.control$lty.line2 <- 1
    # Defaults if no groups are provided
    if(is.null(err.control$col.nogroup)) err.control$col.nogroup <- 1
    if(is.null(err.control$pch.nogroup))
      err.control$pch.nogroup <- 16
    if(is.null(err.control$cex.nogroup))
      err.control$cex.nogroup <- 1
    if(is.null(err.control$lwd.nogroup))
      err.control$lwd.nogroup <- 1
    if(is.null(err.control$transform.to.residuals))
      err.control$transform.to.residuals <- F
    
    
    # Default functions
    if(is.null(err.control$x.bins.function)) err.control$x.bins.function <- function(x)
    {
      round.resolution(x, 1, 0)
    }
    if(is.null(err.control$x.center.function))
      err.control$x.center.function <- function(x)
      {
        mean(x, na.rm = T)
      }
    if(is.null(err.control$y.center.function))
      err.control$y.center.function <- function(x)
      {
        mean(x, na.rm = T)
      }
    if(!is.element("y.low.function", names.given) | is.null(err.control$y.low.function))
      err.control$y.low.function <- function(x)
      {
        ok <- !is.na(x)
        mean(x[ok]) - (var(x[ok])/sum(ok))^0.5
      }
    if(!is.element("y.high.function", names.given) | is.null(err.control$y.high.function))
      err.control$y.high.function <- function(x)
      {
        ok <- !is.na(x)
        mean(x[ok]) + (var(x[ok])/sum(ok))^0.5
      }
    if(err.control$show2) {
      if(is.null(err.control$y.low2.function))
        err.control$y.low2.function <- function(x)
        {
          ok <- !is.na(x)
          mean(x[ok]) - var(x[ok])^0.5
        }
      if(is.null(err.control$y.high2.function))
        err.control$y.high2.function <- function(x)
        {
          ok <- !is.na(x)
          mean(x[ok]) + var(x[ok])^0.5
        }
    }
    # defaults for the text label
    if(is.null(err.control$x.offset.lab)) err.control$x.offset.lab <- 0
    if(is.null(err.control$y.offset.lab.N))
      err.control$y.offset.lab.N <- 0
    if(is.null(err.control$y.offset.lab.center))
      err.control$y.offset.lab.center <- -3
    if(is.null(err.control$srt.lab))
      err.control$srt.lab <- 0
    if(is.null(err.control$cex.lab))
      err.control$cex.lab <- 1
    if(is.null(err.control$adj.lab))
      err.control$adj.lab <- 0
    if(is.null(err.control$col.lab))
      err.control$col.lab <- 1
    if(is.null(err.control$round.lab))
      err.control$round.lab <- 1
    if(is.null(err.control$show.lab.N))
      err.control$show.lab.N <- F
    if(is.null(err.control$show.lab.center))
      err.control$show.lab.center <- F
    if(is.null(err.control$y.lab.function))
      err.control$y.lab.function <- function(x)
      {
        mean(x)
      }
    if(is.null(err.control$lab.code))
      err.control$lab.code <- "paste(\"N=\",length(x))"
    if(!is.element(class(groups), c("factor", "ordered"))) {
      if(trace) {
        print("Minor warning from panel.binned.errorbars: Converted groups to factor(groups).")
        print(paste("Values of factor(groups):", paste(unique(as.character(factor(groups))), collapse = " ")))
      }
      groups <- factor(groups)
    }
    # First get the group colors etc.
    ss <- trellis.par.get("superpose.symbol")
    LL <- trellis.par.get("superpose.line")
    groups.use <- groups[subscripts]
    use.no.group.symbol <- F
    # If no groups have been defined, we simply create one
    if(err.control$pool.groups | is.na(groups[1])) {
      # Create fake group called \"group 1\"
      groups.use <- factor(rep("group 1", length(subscripts)))
      ss <- trellis.par.get("plot.symbol")
      LL <- trellis.par.get("plot.line")
      use.no.group.symbol <- T
    }
    # Loop through all groups
    for(x.bin.no in unique(clanTools::codes.ca(groups.use))) {
      ok <- clanTools::codes.ca(groups.use) == x.bin.no
      x.bin.name <- unique(as.character(groups.use[ok]))
      x.use <- x[ok]
      y.use <- y[ok]
      # Define numerical grouping variable (may be idetical to x)
      x.groups <- err.control$x.bins.function(x.use)
      # Calculate statistics
      N.center <- tapply(x.use, x.groups, function(x)
      {
        ok <- !is.na(x)
        sum(ok)
      }
      )
      x.center <- tapply(x.use, x.groups, err.control$x.center.function)
      
      
      y.center <- tapply(y.use, x.groups, err.control$y.center.function)
      
      y.low <- rep(NA, length(x.center))
      if(!is.null(err.control$y.low.function)) {
        y.low <- tapply(y.use, x.groups, err.control$y.low.function)
      }
      y.high <- rep(NA, length(x.center))
      if(!is.null(err.control$y.high.function)) {
        y.high <- tapply(y.use, x.groups, err.control$y.high.function)
      }
      y.low2 <- rep(NA, length(x.center))
      if(!is.null(err.control$y.low2.function)) {
        y.low2 <- tapply(y.use, x.groups, err.control$y.low2.function)
      }
      y.high2 <- rep(NA, length(x.center))
      if(!is.null(err.control$y.high2.function)) {
        y.high2 <- tapply(y.use, x.groups, err.control$y.high2.function)
      }
      y.lab <- y.center
      if(!is.null(err.control$y.lab.function)) {
        y.lab <- tapply(y.use, x.groups, err.control$y.lab.function)
      }
      
      
      y.group.means <- NA
      
      # Convert logical err.control$transform.to.residuals-values to character-values
      if(!is.character(err.control$transform.to.residuals)){
        if(err.control$transform.to.residuals) err.control$transform.to.residuals<- "abs" else
          err.control$transform.to.residuals <- "nothing"
      }
      
      if(!err.control$transform.to.residuals=="nothing"){
        y.group.means <- tapply(y, err.control$x.bins.function(x), err.control$y.center.function)
        ok2 <- match(names(y.center),names(y.group.means))
        y.group.means <- y.group.means[ok2]
        if(trace){print("group means"); print(y.group.means)}
      }
      
      if(err.control$transform.to.residuals=="abs"){
        y.center <- y.center - y.group.means
        y.low    <- y.low    - y.group.means
        y.high   <- y.high   - y.group.means
        y.low2   <- y.low2   - y.group.means
        y.high2  <- y.high2  - y.group.means
        y.lab    <- y.lab    - y.group.means
      }
      
      if(err.control$transform.to.residuals=="rel"){
        y.center <- (y.center - y.group.means) / y.group.means
        y.low    <- (y.low    - y.group.means) / y.group.means
        y.high   <- (y.high   - y.group.means) / y.group.means
        y.low2   <- (y.low2   - y.group.means) / y.group.means
        y.high2  <- (y.high2  - y.group.means) / y.group.means
        y.lab    <- (y.lab    - y.group.means) / y.group.means
      }
      
      if(err.control$transform.to.residuals=="pct"){
        y.center <- (y.center - y.group.means) / y.group.means * 100
        y.low    <- (y.low    - y.group.means) / y.group.means * 100
        y.high   <- (y.high   - y.group.means) / y.group.means * 100
        y.low2   <- (y.low2   - y.group.means) / y.group.means * 100
        y.high2  <- (y.high2  - y.group.means) / y.group.means * 100
        y.lab    <- (y.lab    - y.group.means) / y.group.means * 100
      }
      
      if(err.control$transform.to.residuals=="norm"){
        y.center <- (y.center ) / y.group.means
        y.low    <- (y.low    ) / y.group.means
        y.high   <- (y.high   ) / y.group.means
        y.low2   <- (y.low2   ) / y.group.means
        y.high2  <- (y.high2  ) / y.group.means
        y.lab    <- (y.lab    ) / y.group.means
      }
      
      if(!is.logical(err.control$transform.to.residuals) & !is.element(err.control$transform.to.residuals,c("nothing","abs","rel","pct","norm"))){
        print("Problem in panel.binned.errorbars.")
        print("transform.to.residuals =")
        print(err.control$transform.to.residuals)
        print("Legal values are: abs, rel, pct, and norm.")
      }
      
      # Look up the scale and convert from inch to mm
      # Old S-plus code: mm.pr.scale <- par("uin") * 25.4
      # From S-plus: usr <- par('usr')
      usr <- c(current.viewport()$xscale, current.viewport()$yscale) 
      pin <- par('pin') # size of plot regions in inches
      mm.dx <- pin[1]*25.4 / (usr[2] - usr[1])
      mm.dy <- pin[2]*25.4 / (usr[4] - usr[3])
      mm.pr.scale <- c(mm.dx, mm.dy)
      
      
      # Set the defaults for points, bars, and lines
      x.width <- err.control$x.width/mm.pr.scale[1]
      x.width2 <- err.control$x.width2/mm.pr.scale[1]
      x.offset <- err.control$x.offset[x.bin.no]/mm.pr.scale[1]
      x.offset.lab <- err.control$x.offset.lab/mm.pr.scale[1]
      y.offset.lab.N <- err.control$y.offset.lab.N/mm.pr.scale[2]
      y.offset.lab.center <- err.control$y.offset.lab.center/mm.pr.scale[2]
      if(is.na(x.offset))
        x.offset <- 0.
      point.radius <- err.control$point.radius/mm.pr.scale[2]
      point.radius2 <- err.control$point.radius2/mm.pr.scale[2]
      col.point <- ss$col[x.bin.no]
      pch.point <- ss$pch[x.bin.no]
      cex.point <- ss$cex[x.bin.no]
      lwd.point <- LL$lwd[x.bin.no]
      if(use.no.group.symbol) {
        col.point <- err.control$col.nogroup
        pch.point <- err.control$pch.nogroup
        cex.point <- err.control$cex.nogroup
        lwd.point <- err.control$lwd.nogroup
      }
      lwd.bar <- err.control$lwd.bar
      col.bar <- err.control$col.bar
      lwd.line <- err.control$lwd.line
      col.line <- err.control$col.line
      lty.line <- err.control$lty.line
      lwd.bar2 <- err.control$lwd.bar2
      col.bar2 <- err.control$col.bar2
      lwd.line2 <- err.control$lwd.line2
      col.line2 <- err.control$col.line2
      lty.line2 <- err.control$lty.line2
      if(col.bar < 0)
        col.bar <- col.point
      if(col.bar2 < 0)
        col.bar2 <- col.point
      if(col.line < 0)
        col.line <- col.point
      if(col.line2 < 0)
        col.line2 <- col.point
      if(err.control$col.lab < 0)
        err.control$col.lab <- col.point
      
      
      # If needed, change values for specific panels
      #if(panel.label=="Test 1") col.point <- 1
      #if(panel.label=="Test 3") col.point <- 1
      # If needed, change values for specific groups
      #if(x.bin.name=="Test 1") {x.offset <- 0/mm.pr.scale[1]};
      #if(x.bin.name=="Test 3") {x.offset <- 0/mm.pr.scale[1]};
      # Plot vertical lines (from center to low or high point)
      
      
      if(err.control$show2) {
        lsegments(x.center + x.offset, pmin(y.high2, y.center + point.radius2, na.rm = T), x.center + x.offset, y.high2, lwd =
                    lwd.line2, col = col.line2, lty = lty.line2)
        lsegments(x.center + x.offset, pmax(y.low2, y.center - point.radius2, na.rm = T), x.center + x.offset, y.low2, lwd = lwd.line2,
                  col = col.line2, lty = lty.line2)
      }
      if(err.control$show) {
        lsegments(x.center + x.offset, pmin(y.high, y.center + point.radius, na.rm = T), x.center + x.offset, y.high, lwd = lwd.line,
                  col = col.line, lty = lty.line)
        lsegments(x.center + x.offset, pmax(y.low, y.center - point.radius, na.rm = T), x.center + x.offset, y.low, lwd = lwd.line,
                  col = col.line, lty = lty.line)
      }
      # Plot horizontal bars (at the low or high point)
      if(err.control$show2) {
        lsegments(x.center - x.width2 + x.offset, y.high2, x.center + x.width2 + x.offset, y.high2, lwd = lwd.bar2, col = col.bar2)
        lsegments(x.center - x.width2 + x.offset, y.low2, x.center + x.width2 + x.offset, y.low2, lwd = lwd.bar2, col = col.bar2)
      }
      if(err.control$show) {
        lsegments(x.center - x.width + x.offset, y.high, x.center + x.width + x.offset, y.high, lwd = lwd.bar, col = col.bar)
        lsegments(x.center - x.width + x.offset, y.low, x.center + x.width + x.offset, y.low, lwd = lwd.bar, col = col.bar)
      }
      
      
      # Plot center points
      if(err.control$show.center) {
        lpoints(x.center + x.offset, y.center, col = col.point, cex = cex.point, pch = pch.point, lwd = lwd.point, type = err.control$
                  type)
      }
      if(err.control$show.lab.N) {
        txt <- paste(
          "ltext(x.center+x.offset+x.offset.lab, y.lab+y.offset.lab.N,N.center,adj=err.control$adj.lab, cex=err.control$cex.lab, srt=err.control$srt.lab, col=err.control$col.lab)"
        )
        eval(parse(text = txt))
      }
      if(err.control$show.lab.center) {
        txt <- paste(
          "ltext(x.center+x.offset+x.offset.lab, y.lab+y.offset.lab.center,round.ca(y.center,err.control$round.lab),adj=err.control$adj.lab, cex=err.control$cex.lab, srt=err.control$srt.lab, col=err.control$col.lab)"
        )
        eval(parse(text = txt))
      }
      
    }
    if(trace)
      wrline("panel.binned.errorbars (end)")
    
  } # End panel.binned.errorbars
