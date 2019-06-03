\name{spnFree}
\title{Release of Spectrino}
\usage{
spnFree(inclApp)
}
\alias{spnFree}
\arguments{
\item{inclApp}{ - logical (default is FALSE) option to include (or not) the release of Spectrino application as well}
}
\description{
  Release R-object and closes application of Spectrino (optionally) after you have finished working with it. 
That is the proper way to close Spectrino, closing only the application will leave R-object of Spectrino.
}
\value{
  \code{spnFree} - Returns nothing.
}
\author{Teodor Krastev}
\examples{
# Initialization of Spectrino
spnNew() 

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}