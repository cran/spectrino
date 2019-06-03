\name{spnCheck}
\title{Check spectrino object existance}
\usage{
spnCheck(inclApp = FALSE)
}
\alias{spnCheck}
\arguments{
\item{inclApp}{ - logical (default is FALSE) option to include (or not) the connection to the application in the verification}
}
\description{
  Check for spectrino object existance. If the object is not there, you probably missed to create 
it with spnNew(...) or for some reason it has been destroyed. If you are checking for the application too, 
the function will check only for existing connection to the application.
}
\value{
  \code{spnCheck} returns logical for spectrino object/app existance. 
}
\author{Teodor Krastev}
\examples{
# Initialization of Spectrino
spnNew() 

spnCheck(TRUE)

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}