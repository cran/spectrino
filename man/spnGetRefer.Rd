\name{spnGetRefer}
\title{Get common X values in a vector of the active group in the top tab/spec-tree}
\usage{
spnGetRefer()
}
\alias{spnGetRefer}
\description{
  Get reference X set of values (vector). All the spectra in a group list are assumed to have common X set of values, so if there is loaded spectrum in different X values, the spectrum is recalculated to fit the set given by the options: Boundaries from Low to High by 1. 
}
\value{
  \code{spnGetRefer} returns the reference X set of values (vector) of the active group in the top tab/spec-tree
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnGetSpc} ,\link{spnGetGrp} , \link{spnGetTree}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# the reference X values
v1 <- spnGetRefer() 

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}