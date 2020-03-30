clanLattice
===========

Functions using the graphical package in R called Lattice. 

To install this package do the following:

(1) First get the devtools package, if you do not already have it:

install.packages("devtools")

library(devtools)



(2) Then get the clanLattice package from github:

install_github("claus-e-andersen/clanLattice")

library(clanLattice)


(3) To get a list of functions in the library, just call:

?clanLattice

and click at index link at the bottom of the page.

(4) To actually try something, there are the folowing demonstration functions:

require(lattice)
require(latticeExtrs)
require(grid)

txtplot.demo() 
panel.binned.errorbars.demo() 
panel.ebars.demo()


