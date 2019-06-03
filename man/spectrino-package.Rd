\name{spectrino-package}
\alias{spectrino-package}
\alias{spectrino}
\docType{package}
\title{
Spectra viewer, organizer, data preparation and property blocks
}
\description{
Spectra viewer, organizer, data preparation and property blocks from within R or stand-alone. Binary (Spectrino application) part is installed separately.

}
\details{
\tabular{ll}{
Package: \tab spectrino\cr
Type: \tab Package\cr
Version: \tab 2.0.0\cr
Date: \tab 2019-04-15\cr
License: \tab MIT license\cr
}
Installation:
This is a binary package, Spectrino application (Windows) is integral part of the package. 
If you install it as a binary package (spectrino***.zip) all the components will be installed. 
If you install it as a source package from CRAN or some CRAN mirror (spectrino***.gz) only the R part 
will be installed, you have to run spnInstallApp() in order to install the application part from 
http://spectrino.com website.     

Running:
After creating spectrino object in R and opening spectrino application by using spnNew function,
you can manipulate, select/deselect, open/close/save - individual spectra/group of spectra/tree of groups.
The manipulation features are complete enough.
After finishing with the package you should free the Spectrino object and depending on "inclApp" parameter
Spectrino application will be closed or not as well.

General features:

Spec part:
for the spec part of Spectrino common point are:
- all indexes of items (spec-tree, spec-group and specs) are 1-based and 0 is resertved for the active one. For the active spec-tree is the top one, the active group is the selected one and the active  
- common notation for mask is "*" or "<ALL>" which mask all item in the designated list
- when you refer to spec-tree, spec-group or spec, you can do that by number or by name. That's why it is not a good idea to name any item with a number (even you operation system would allow it).

Block part:
- a core of block is list of properties, you can read/write the values of these properties from your code or from the user interface. 
- the four types of property are: boolean, integer, double and string. You create or modify the types and some other staff from configration dialog.
- there are three other parts of a block: chart, log and source. Chart will follow the evolution of any numerical property in a graph. Log will do the same and more in text format. Source will keep multi-line pieces (up to 3) of text you can read from R as text or execute if it is an excutable R code.  
}
\author{
Teodor Krastev <spectrino@sicyon.com>

Maintainer: Teodor Krastev <spectrino@sicyon.com>
}
\references{
Teodor Krastev, Journal of Statistical Software" (v18, 2007)
}
\examples{
# Initialization of Spectrino
spnNew() 

# get the number of active group
#i <- spnActGrp(0) 

# Release of Spectrino object (optional)
#spnFree()
}
