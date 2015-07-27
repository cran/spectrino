\name{spectrino-package}
\alias{spectrino-package}
\alias{spectrino}
\docType{package}
\title{
Spectra visualization, organizer and data preparation
}
\description{
Spectra visualization, organizer and data preparation
        from within R or stand-alone
}
\details{
\tabular{ll}{
Package: \tab spectrino\cr
Type: \tab Package\cr
Version: \tab 1.6.0\cr
Date: \tab 2015-03-15\cr
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

# generate test set
spnOpenTree("<test>")

# set third group to be active
spnActGrp(3)

# get the number of active group
i <- spnActGrp(0) 

# Release of Spectrino object
spnFree()
}
