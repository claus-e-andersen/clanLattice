clanLattice
===========

clanLattice is an R package. The purpose of the package is to provide functions that facilitate creation of graphs using the 
Lattice graphical system in R. Lattice is a panel-based, high-level data visualization system mainly authored by Deepayan Sarkar [1].
The system is a re-implementation and improvement of Trellis plots from S-plus which in turn was designed based on original research by Bill Cleveland from Bell Labs [2]. 

The use of panel functions in Lattice plots means that it is easy to present specific modelling results or other supplementary information within each panel. 

An important function in the clanLattice package is txtplot() which enables production of
automated measurement reports consisting of a combination of graphical plots, text, and tables with dataframe output. 
The system enables detailed formatting (e.g. number of significant digits) of numerical output in 
tables as required in technical reports. The formatting is carried out using a pretty-function written by the user allowing for complete control over how each individual number should be formated.  

The clanLattice package also contains special panel functions for plotting of errorbars (uncertainty bars). An important feature is that errorbars can be offset from the original data, which is useful if you want to see both both or if you need to distinguish otherwise overlapping errorbars. 

The clanLattice package is used extensively for production of automated measurement reports in the context of metrology research and calibrations under DS/EN ISO/IEC 17025:2017 accreditation (see www.mrdc.dtu.dk). 

Examples of plots are in the pdfs-folder:
https://github.com/claus-e-andersen/clanLattice/tree/master/pdfs

[1] Deepanyan Sarkar: "Lattice. Multivariate data visualization with R" (Spring, 2008).
[2] William S. Cleveland: "The elements of graphing data" (Hobart Press, 1994).

-------------

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

