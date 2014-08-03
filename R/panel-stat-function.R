#' @title Lattice panel for adding summary statistics to graphs
#' @description This function facilitates integration of summary statistics
#' into lattice graphs. Requires grid, lattice and clanTools.
#' @usage
#'  library(grid)
#'  library(lattice)
#'  library(clanTools)
#'  df.dum <- data.frame(dose=1:100, response=rnorm(100,mean=10,sd=0.1))
#'  xyplot(response ~ dose,
#'     panel=function(x,y,...){
#'     panel.xyplot(x,y,...)
#'     panel.stat(y)
#'     panel.stat(y,head="Summary:",unit="mSv",decimals=5,cx=0.65,dy=0.05)
#'  },
#'  data=df.dum)
#' @name panel.stat
#' @author Claus E. Andersen
#' @return A lattice panel. 
#' @param xx is the vector of number to be analyzed
#' @param cx is the x-position within the plot (0 - 1)
#' @param cy is the y-position within the plot (0 - 1)
#' @param dx is the increment in x coordinate from line to line (normally 0)
#' @param dy is the increment in x coordinate from line to line
#' @param just is the justification 
#' @param decimals isi the number of decimals
#' @param head is the title of the statistics
#' @param unit is the unit to be used 
#' @param select is a a 7-element vector controlling what statistics should be
#' shown: 
#'    Element 1: Mean
#'    Element 2: Sdandard deviation
#'    Element 3: Relative standard deviation in %
#'    Element 4: N is the number of data points
#'    Element 5: Max is the maximum
#'    Element 6: Min is the minimum
#'    Element 7: Range (i.e. min to max)
#' @param ... is any additional formatting parameters 
#' @export panel.stat
panel.stat <- function(xx,cx=0.1,cy=0.9,dx=0,dy=0.1,
                       just='left',
                       decimals=3,head=NA,unit="",
                      select=c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE),...){
  # Created: August 2011
  # Revised: September 17, 2011
  # Name   : Claus E. Andersen
  # Objective: To facilitate integration of summary statistics
  #            in lattice graphs.
  # Sample call:
  #   df.dum <- data.frame(dose=1:100, response=rnorm(100,mean=10,sd=0.1))
  #  xyplot(response ~ dose,
  #     panel=function(x,y,...){
  #     panel.xyplot(x,y,...)
  #     panel.stat(y)
  #     panel.stat(y,head="Summary:",unit="mSv",decimals=5,cx=0.65,dy=0.05,select=c(T,F,F,F,T,T,T))
  #   },
  #   data=df.dum)
  # 
  cx0 <- cx
  cy0 <- cy
  if(!is.na(head)){grid.text(head,cx0,cy0,just=just,...); cx0 <- cx0 + dx; cy0 <- cy0 - dy;} 
  if(select[1]){
    grid.text(paste('Mean =',round.ca(mean(xx),decimals),unit),cx0,cy0,just=just,...)
    cx0 <- cx0 + dx; cy0 <- cy0 - dy
  }
  
  if(select[2]){
    grid.text(paste('Sd =',round.ca(sd(xx),decimals),unit),cx0,cy0,,just=just,...)
    cx0 <- cx0 + dx; cy0 <- cy0 - dy
  }
  
  if(select[3]){
    grid.text(paste('Rel. sd =',round.ca(sd(xx)/mean(xx)*100,decimals),'%'),cx0,cy0,,just=just,...)
    cx0 <- cx0 + dx; cy0 <- cy0 - dy
  }
  
  if(select[4]){
    grid.text(paste('N =',length(xx)),cx0,cy0,just=just,...)
    cx0 <- cx0 + dx; cy0 <- cy0 - dy
  }
  
  if(select[5]){
    grid.text(paste('Max =',round.ca(max(xx),decimals),unit),cx0,cy0,,just=just,...)
    cx0 <- cx0 + dx; cy0 <- cy0 - dy
  }
  
  if(select[6]){
    grid.text(paste('Min =',round.ca(min(xx),decimals),unit),cx0,cy0,,just=just,...)
    cx0 <- cx0 + dx; cy0 <- cy0 - dy
  }
  
  if(select[7]){
    grid.text(paste('Range =',round.ca(max(xx)-min(xx),decimals),unit),cx0,cy0,,just=just,...)
    cx0 <- cx0 + dx; cy0 <- cy0 - dy
  }
} # panel.stat

