#' @title Reorder a factor such that lattice plots come out in the correct numerical order.
#' @description Reorder a factor such that lattice plots come out in the correct numerical order. This
#' function is useful when lattice plots are conditioned using factors with a mixture of text and numbers.
#' @usage 
#'   reorder.for.trellis(c("OSL01 - 5 s", "OSL02 - 200", "OSL03 - -100.3", "A03 - -100.3 s"))
#' @name reorder.for.trellis
#' @author Claus E. Andersen
#' @return A reordered vector
#' @param x is the vector to be reordered
#' @param N=1 means the first number (ignoring text etc.) N=2 means the second number (ignoring text etc.)
#' @param method is the methiod to be used. Only "Nth-number" has been implemented
#' @param absolute use absolute values. This avoids problems for text like temp-25-degC where
#' 25 is probably 25 degC rather than -25 degC.
#' @param reverse: If true the order is reversed. 
#' @export reorder.for.trellis
reorder.for.trellis <- function(x, N = 1, method = "Nth-number", absolute = FALSE, reverse = FALSE){
  # Create a factor out of x (it may alraedy be a factor) and order the levels in accordance with the
  # N'th number in x (vectorized). N=1 means the first number (ignoring text etc.) N=2 means the
  # second number (ignoring text etc.)
  # See also: reorder.factor.by.length"
  # Created: July 13, 2006
  # Revised: August 2, 2007 (Added the arguments: reverse and absolute)
  # Claus E. Andersen
  # Sample call:
  ##   reorder.for.trellis(c("OSL01 - 5 s", "OSL02 - 200", "OSL03 - -100.3", "A03 - -100.3 s"),1)
  ##   [1] OSL01 - 5 s    OSL02 - 200    OSL03 - -100.3 A03 - -100.3 s
  ##   OSL01 - 5 s < OSL02 - 200 < A03 - -100.3 s < OSL03 - -100.3
  ##
  ##   reorder.for.trellis(c("OSL01 - 5 s", "OSL02 - 200", "OSL03 - -100.3", "A03 - -100.3 s"),2)
  ##   [1] OSL01 - 5 s    OSL02 - 200    OSL03 - -100.3 A03 - -100.3 s
  ##   A03 - -100.3 s < OSL03 - -100.3 < OSL01 - 5 s < OSL02 - 200
  sign <- 1.
  if(reverse)
    sign <- -1.
  x <- as.character(x)
  x.num <- extract.given.number(x, N)
  if(absolute)
    x.num <- abs(x.num)
  return(reorder.factor(factor(x), sign * x.num))
}

