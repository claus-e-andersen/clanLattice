clanLattice
===========

The clanLattice package contains functions that facilitates creation of graphs using the 
Lattice package.   

An important function in this package is txtplot() which enables production of
automated measurement reports consisting of both text and lattice plots. 

Examples of plots are in the pdfs-folder:
https://github.com/claus-e-andersen/clanLattice/tree/master/pdfs


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

(4) To actually try something, there are the folowing demonstration functions (to run the demos you will need the following packages installed):

require(clanLattice)   

require(clanTools)

require(lattice)

require(latticeExtra)

require(grid)

require(dplyr)



txtplot.demo()

panel.binned.errorbars.demo() 

panel.ebars.demo()

trellis.residual.plot.demo()

trellis.residual.plot.demo()

