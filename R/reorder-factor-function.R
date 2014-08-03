#' @title Reorder a vector of type factor 
#' @description  Reorder a vector of type factor in accordance with the
#' results of groupwise results (e.g. mean values) for the different 
#' levels in the factor. 
#' @usage
#' show.trellis.symbols(palette(),"default palette")
#' @name reorder.factor
#' @author Claus E. Andersen
#' @return A reordered factor
#' @param Factor is the vector of factors to be reordered
#' @param x is a vector of numerical values with the same length as Factor.
#' @param Function is a function used on the aggregared data in x. The reordering will be
#' based on these results.
#' @param ... Any other values that could be supplied to Function
#' @export reorder.factor
reorder.factor <- function(Factor, X, Function = mean, ...){
  return(ordered(Factor, levels(Factor)[order(tapply(X, Factor, Function, ...))]))
}
