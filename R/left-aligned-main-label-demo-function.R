#' @title Demonstration how tocreate a lattice plot with a left-aligned main label
#' @description This functions creates a left-aligned main label
#' using a textgrob as given below.
#' How did I get that idea? See Lattice book p.152 / sec. 9.2.1 + demo("labels",package="lattice")
#' @usage
#' require(lattice)
#' require(grid)
#' plt <- left.aligned.main.label.demo()
#' print(plt)
#' @name left.aligned.main.label.demo
#' @author Claus E. Andersen
#' @return A Lattice plot 
#' @export left.aligned.main.label.demo
left.aligned.main.label.demo <- function(){

main.txt <- 
"Ionization chamber kQ-results (grouped) stratified by beam quality, model type and traceability 
Errorbars show standard uncertainties for one single ionization chamber (k=1)
IAEA TRS-398 generic data = grey line. 
IAEA TRS-398 new data (2019) = green line."
  
plt <- xyplot(Sepal.Length ~ Sepal.Width,
                par.strip.tex=list(cex=1.1),
                # To produce a left-aligned main label we use a textgrob as given below.
                # How did I get that idea? See Lattice book p.152 / sec. 9.2.1 + demo("labels",package="lattice")
                # The label can be adjusted further with the mm-units given below. 
                main = grid::textGrob(main.txt, 
                                just="left",
                                x = grid::unit(0.0, "npc") + grid::unit(c(4), "mm"),
                                y = grid::unit(0.0, "npc") + grid::unit(c(2), "mm"),
                                gp = grid::gpar(col = c("black"), cex = 0.8)),
                
                data=iris)
  return(plt)
}# left.aligned.main.label.demo
