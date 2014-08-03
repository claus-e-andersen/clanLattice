#' @title Version function for the clanLattice library
#' @description Version function for the clanLattice library.
#' @usage
#' clanLattice()
#' @name clanLattice
#' @author Claus E. Andersen
#' @return A list of information about the version and functions within clanLattice.
#' @export 
clanLattice <- function(){
  list(name="clanLattice",
       version=0.008,
       date="August 3, 2014",
       functions=sort(c("clanLattice"
       )))
}