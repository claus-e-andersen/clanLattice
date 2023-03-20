# What is clanLattice?
clanLattice is an R package. The purpose of the package is to provide functions that facilitate creation of graphs using the 
lattice graphical system in R. 

# What is lattice graphics?
Lattice is a panel-based, high-level data visualization system authored by Deepayan Sarkar [1] using grid graphics [2]. The Lattice system is a re-implementation and improvement of Trellis plots from S-plus which in turn were designed based on original research by Bill Cleveland from Bell Labs [3]. 

The use of panel functions in Lattice plots means that it is easy to present specific modelling results or other supplementary information within each panel. 

# What are the main features of clanLattice?
An important function in the clanLattice package is txtplot() which enables production of
automated measurement reports consisting of a combination of graphical plots, text, and tables with dataframe output. 
The system provides detailed formatting capabilities of numerical output in 
tables as required in technical reports. The formatting is carried out using a so-called pretty-function written by the user allowing for complete control over how each individual number should be formated (e.g. the number of digits printed conditional on the type of data, the column in which the data point appears, and the numerical value of the data point). See Example-txtplot-demo.pdf and the accociated R-function txtplot.demo(). 

The clanLattice package also contains special panel functions for plotting of errorbars (uncertainty bars). An important feature is that errorbars can be offset from the original data, which is useful if you want to see both or if you need to distinguish otherwise overlapping errorbars. 

# Main use case?
I use the clanLattice package extensively for production of automated measurement reports in the context of metrology research and calibrations under DS/EN ISO/IEC 17025:2017 accreditation (see www.mrdc.dtu.dk). 

Examples of plots are in the pdfs-folder:
https://github.com/claus-e-andersen/clanLattice/tree/master/pdfs

# References
[1] Deepanyan Sarkar: "Lattice. Multivariate data visualization with R" (Spring, 2008).
[2] Paul Murrell: "R graphics (3rd ed)" (CRC Press, 2019).
[3] William S. Cleveland: "The elements of graphing data" (Hobart Press, 1994).

# How to install clanLattice?
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

