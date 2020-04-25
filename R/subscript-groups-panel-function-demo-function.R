#' @title Demonstration of how to write a panel function (subscripts and groups)
#' @description This function demonstrate how to write a panel function that 
#' takes care of subscripts and groups. The main trick is to always use:
#' 
#'  panel = function(x, y, subscripts, groups,...){ }
#'  
#' subscripts are always available, but groups may not be, so to mitigate that
#' simply use:
#' 
#'               if(missing(groups))\{
#'               panel.xyplot(x, y, ...)\} else \{
#'               panel.superpose(x, y, subscripts,groups, ...)
#'               \}#if 
#'               
#' The use of \code{panel.number()}, \code{current.row}, and \code{current.column()} is also demonstrated.               
#' @usage
#'  require(grid)
#'  require(lattice)
#'  pp <- panel.function.demo()
#'  pp
#'  pp[[3]]
#' @name panel.function.demo
#' @author Claus E. Andersen
#' @return A Lattice plot
#' @export panel.function.demo
panel.function.demo <- function(){
names(iris)


plt1 <- xyplot(Sepal.Length ~ Sepal.Width | Species,
               par.strip.text=list(cex=1.5),
               data=iris,
               panel=function(x, y, subscripts, groups,...){
                 panel.xyplot(x,y,...)
               }# panel function
)# xyplot


df.outside <- NULL

plt2 <- xyplot(Sepal.Length ~ Sepal.Width|Species,
               data=iris,
               panel=function(x, y, subscripts, groups,...){
                 
                 # Tell me where we are
                 print(paste("Panel no. = ",panel.number()))
                 print(paste("Row no. = ",current.row()))
                 print(paste("Column no. = ",current.column()))
                 
                 cex0 <- 0.8
                 grid::grid.text(paste("SD =",sprintf("%.5f", round(sd(y),5))) ,     x = grid::unit(0.1, "npc"), y = grid::unit(0.85, "npc"), just=0, gp=grid::gpar(cex=cex0))
                 
                 # Demonstration of the use of subscripts:
                 x.Specises <- paste(unique(iris$Species[subscripts]),collapse="")
                 print(x.Specises)
                 
                 if(missing(groups)){
                   panel.xyplot(x, y, ...)} else{
                     panel.superpose(x, y, subscripts,groups, ...)
                   }#if 
  
                 
                 # How to get data in and out of the panel function
                 df0 <- data.frame(x.mean=mean(x), y.mean=mean(y), panel.no = panel.number(),Specises=x.Specises,N=length(x))
                 
                 df.out <- get("df.outside",envir=sys.frame(0))
                 if(is.null(df.out)){df.out <- df0} else {df.out <- rbind(df.out,df0)}
                 assign("df.outside",df.out,envir=sys.frame(0))
                 
                 
                                
               }# panel function
)# xyplot

plt1
plt2
plt3 <- update(plt2, groups=iris$Species)
plt3
list(plt1,plt2,plt3)

txtplot(df.outside)
print(df.outside)
}# panel.function.demo