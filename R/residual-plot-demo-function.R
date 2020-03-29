#' @title Function for demonstrating how to make residual plots in lattice
#' @description This function uses several features of the latticeExtra package
#' to make plots with one large panel for the main data and a much smaller plot
#' above for the residuals. The data may me split into different levels. In the
#' specific example there are three laboratories, and so the final plot
#' consists of 3 x 2 panels. The main plots have the same scale, and likewise for 
#' the residual panels.  
#' @usage
#' plt <- trellis.residual.plot.demo()
#' print.trellis.plots(list(plt),y.size=c(0.2,0.8))
#' @name trellis.residual.plot.demo
#' @author Claus E. Andersen
#' @return A Lattice plot 
#' @export trellis.residual.plot.demo
trellis.residual.plot.demo <- function(){
  # Function for demonstrating how to make residual plots
  ###############################################
  # First make some synthetic data
  ###############################################
  N <- 1000
  df <- data.frame(Gy.ref = seq(1,100,length=N),Gy.meas=0.0, Gy.residuals=0.0,
                   pct.residuals=0.0, lab=sample(c("LNHB","NPL","PTB"),N,replace=TRUE))
  
  df %>%
    group_by(lab) %>%
    mutate(sd.lab = 0.005) %>%
    mutate(sd.lab = ifelse(lab=="NPL",0.01,sd.lab)) %>%
    mutate(sd.lab = ifelse(lab=="PTB",0.02,sd.lab)) %>%
    mutate(sd.lab = ifelse(lab=="LNHB",0.03,sd.lab)) %>%
    mutate(Gy.meas = Gy.ref * rnorm(n(),mean=1,sd=sd.lab)) %>%
    ungroup(.) %>%
    mutate(Gy.residual = Gy.meas - Gy.ref) %>%
    data.frame(.) -> df
  
  df.stack <- stack.for.trellis(df,c("Gy.meas","Gy.residual"),"Gy")
  
  
  ###############################################
  # Scales
  ###############################################
  # First scales for the main and the residual panels
  # N is the number of columns
  N <- 3
  ylim.main <- c(0,110)
  ylim.res  <- c(-20,20)
  ylim0 <- c(rep(list(ylim.main),N),rep(list(ylim.res),N))
  
  ###############################################
  # Main plot
  ###############################################
  plt.01 <- xyplot(Gy ~ Gy.ref | lab + ordered(which,levels=c("Gy.meas","Gy.residual")),
                   data=df.stack,
                   par.strip.text=list(cex=0.6),
                   between=list(x=1,y=0.3),
                   ylim = ylim0,
                   scales = list(y = list(relation="free", tick.number = 5, rot = 0)),
                   panel=function(x,y,...){
                     panel.xyplot(x,y,...)
                     
                     cex0 <- 0.7
                     
                     if(current.row() %in% 1){
                       # Main data
                       panel.abline(lm(y~x))
                       panel.abline(h=0,lty='dashed')
                       panel.abline(v=0,lty='dashed')
                       grid.text(paste("Mean =",sprintf("%.5f", round(mean(x),5))),  x = unit(0.1, "npc"), y = unit(0.95, "npc"),just=0,gp=gpar(cex=cex0))
                       grid.text(paste("SD =",sprintf("%.5f", round(sd(x),5))) ,     x = unit(0.1, "npc"), y = unit(0.85, "npc"),just=0,gp=gpar(cex=cex0))
                       grid.text(paste("N =",length(x)),                             x = unit(0.1, "npc"), y = unit(0.75, "npc"),just=0,gp=gpar(cex=cex0))
                     }# Main panels
                     
                     if(current.row() %in% 2){
                       # Residuals
                       panel.abline(h=0)
                       panel.abline(v=0,lty='dashed')
                       grid.text(paste("SD =",sprintf("%.5f", round(sd(y),5))) ,     x = unit(0.1, "npc"), y = unit(0.85, "npc"),just=0,gp=gpar(cex=cex0))
                     }# Residual panels
                     
                   } # panel function
  )
  
  ###############################################
  # LatticeExtra tricks
  ###############################################
  plt.02 <- useOuterStrips(plt.01)
  
  lat.opt <- list(
    layout.widths=list(strip.left=list(x=1.4)),
    layout.heights=list(strip=list(x=c(1.4)), panel=list(x=c(3,1)))
  )
  plt.03 <- update(plt2,lattice.options=lat.opt)
  
  plt.04 <- combineLimits(plt.03)
  
  if(FALSE){print.trellis.plots(list(plt.04),y.size=c(0.2,0.8))}
  
  return(plt.04)
}# trellis.residual.plot.demo 

