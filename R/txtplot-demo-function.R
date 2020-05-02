#' @title This function provides demonstrations of the use of texplot 
#' @description Demonstrations of how txtplot can be used for presentationof
#' text and dataframe.
#' @usage
#' txtplot.demo()
#' @name txtplot.demo
#' @author Claus E. Andersen
#' @return Lattice plots.
#' @export txtplot.demo
txtplot.demo <- function(){
  # Created  : October 30, 2011
  # Revised  : October 30, 2011
  # Name     : Claus E. Andersen  
  #            Technical University of Denmark
  # Objective: to demonstrate the use of texplot 
  # Sample call: 
  #   txtplot.demo()
  
  ###################################################################  
  print("Intro page with box")
  ###################################################################  
  txt <- 
    "
  txtplot examples in R
  Claus E. Andersen
  October 30, 2011
  "
  PP <- list(0.05, 0.9, 1)
  PP <- txtplot(txt, cex=3, T, PP, line.size=80, box=list(c(0,1,0.3,0.85),lty=1,lwd=6,col='blue'))
  
  
  ###################################################################  
  print("Simplest possible example")
  ###################################################################  
  txt <- 
    "
  # Example of txtplot output that will be readable both in the
  # script and in report together with all the graphs:
  
  This function can print text or dataframes on a separate graphsheet or on an 
  existing graph. The main idea behind the function is to add experimental or 
  analytical details (=text) on separate 'graphs' in between the real xy-graphs. 
  When everything is exported to pdf or PowerPoint, we both have explanations and 
  graphs in the same document. At the same time, we can easily read the text in 
  the script. Hence this function should facilitate improved documentation. 
  This is kind of a simple way of literate programming. 
  
  # Contact: clan@risoe.dtu.dk
  
  # We first fitted this model to the date:
  #   response ~ dose + dose^2  
  # and this resulted in...
  "
  txtplot(txt)
  
  ###################################################################  
  print("Text and dataframes")
  ###################################################################  
  PP <- NULL
  txt <- "
  Demonstration of how to combine text and dataframes with
  some control over resolution or number of significant digits
  being printed. First, default:
  "
  PP <- txtplot(txt,new=T,PP)
  N <- 5
  df <- data.frame(id=sample(c("Varian EX21","Siemens","Volvo 100"),N,replace=T),x=1:N,y=1:N,dose.Gy=0.01*rnorm(N),response.counts=1000*rnorm(N))
  PP <- txtplot(df,new=F,PP,cex=0.6)
  
  PP <- txtplot("\nThen use the pretty-function with five significant digits for all numerical columns:\n",new=F,PP)
  pretty.func2 <- function(x, col.num = 1){if(is.numeric(x))signif(x,5)else x}
  PP <- txtplot(df, new=F, PP, cex=0.6, pretty.func = pretty.func2)
  
  PP <- txtplot("\nAnd finally a print with individual rounding for specificcolumns:\n",new=F,PP)
  pretty.func2 <- function(x, col.num = 1){
    res <- x
    if(is.numeric(x)){
      res <- signif(x,5)
      if(col.num==2){res <- clanTools::round.ca(x,1)} 
      if(col.num==3){res <- clanTools::round.ca(x,1)} 
      if(col.num==4){res <- clanTools::round.ca(x,6)} 
      if(col.num==5){res <- clanTools::round.ca(x,1)}
    } 
    res
  }
  PP <- txtplot(df, new=F, PP, cex=0.6, pretty.func = pretty.func2)
  
  ###################################################################  
  print("Text and dataframes (keeping track of x-position between two txtplot-calls)")
  ###################################################################  
  df <- data.frame(x=1:5, y=rnorm(5),name="Hi there")
  #  Notice: x is used to keep track of the current location on the graph.
  x <- txtplot(" The main results calculated with\n",T,list(0.1,0.8,1))
  x <- txtplot(" RnMod3d.pas",F,x)
  x <- txtplot(" or some other codes:\n",F,x,maintain.start.pos=F,col='blue')
  x[[1]] <- 0.1
  x <- txtplot(df, F, x, col='red', cex=1, pretty.func = function(x, col.num = 1){if(is.numeric(x))signif(x,2)else x}, table.lines=c(1,2,999))
  
  ###################################################################  
  print("Dataframes with many rows")
  ###################################################################  
  PP <- NULL
  txt <- "
  Demonstration of how to print a long table (70 lines):
  "
  PP <- txtplot(txt,new=T,PP)
  N <- 70
  df <- data.frame(id=sample(c("Varian EX21","Siemens","Volvo 100"),N,replace=T),x=1:N,y=1:N,dose.Gy=0.01*rnorm(N),response.counts=1000*rnorm(N),no=1:N)
  pretty.func2 <- function(x, col.num = 1){if(is.numeric(x))signif(x,5)else x}
  PP <- txtplot(df, new=F, PP, cex=0.6, pretty.func = pretty.func2)
  
  
  ###################################################################  
  print("Graphs and dataframes")
  ###################################################################  
  N <- 10
  df <- data.frame(id=sample(c("Varian EX21","Siemens","Volvo 100"),N,replace=T),x=1:N,y=1:N,dose.Gy=abs(10*rnorm(N)),response.counts=1000*rnorm(N))
  df$response.counts <- df$dose.Gy / 1000
  
  grid.newpage()
  pushViewport(viewport(x=unit(0.5,"npc"),y=unit(0.5,"npc"),width=unit(1,"npc"),height=unit(1,"npc"),name="vp.mine"))
  
  plt <- xyplot(dose.Gy ~ response.counts , data=df, type="b",cex=3)
  print(plt, more=F, newpage=F)
  
  PP <- list(0.1,0.9,1)
  PP <- txtplot("Table with key results:\n", new=F, PP)
  pretty.func2 <- function(x, col.num = 1){if(is.numeric(x))clanTools::round.ca(x,5)else x}
  PP <- txtplot(df[,c(4,5)], new=F, PP, cex=0.6, pretty.func=pretty.func2)
  
  
  ###################################################################  
  print("End of txtplot demonstration (see plots)")
  ###################################################################  
  txt <- 
    "
End of txtplot demonstration
(scroll back to see plots)
"
  PP <- list(0.05, 0.9, 1)
  PP <- txtplot(txt, cex=2.5, T, PP, line.size=80, box=list(c(0,1,0.3,0.85), lty=1, lwd=6, col='blue'))
} # End of txtplot.demo







