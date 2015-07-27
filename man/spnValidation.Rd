\name{spnValidation}
\title{Validation of Spectrino}
\usage{
spnValidation()
}
\alias{spnValidation}
\description{
  Spectrino validation - not conclusive, tests only the most common functions and modes.
}
\value{
  \code{spnValidation} - If it gets back "Validation confirmed" you have very good chances that Spectrino might work, otherwise you will have the error with a number (see the code).
}
\author{Teodor Krastev}
\examples{
# Initialization of Spectrino 
spnNew() 

spnValidation()

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}