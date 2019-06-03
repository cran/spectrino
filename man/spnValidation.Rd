\name{spnValidation}
\title{Validation of Spectrino}
\usage{
spnValidation(Spec, Block)
}
\alias{spnValidation}
\arguments{
\item{Spec}{ - logical (default is TRUE) option to include (or not) a test of most used spectral commands}
\item{Block}{ - logical (default is TRUE) option to include (or not) a test of most used block of prop. commands}
}

\description{
  Spectrino validation - not conclusive, it tests only the most common functions and modes. If both Spec and Block are FALSE, it tests only the Spectrino object, connection and Spectrino app. presence.
}
\value{
  \code{spnValidation} - If it gets back "Validation confirmed" you have very good chances that Spectrino might work, otherwise you will have the error with a number (see the code).
}
\author{Teodor Krastev}
\examples{
# Initialization of Spectrino 
spnNew() 

# test of the Spectrino object, connection and Spectrino app. presence
spnValidation(FALSE,FALSE)

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}