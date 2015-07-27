\name{spnSetVis}
\title{Set Spectrino to be visible or hidden}
\usage{
spnSetVis(Visible)
}
\alias{spnSetVis}
\arguments{
\item{Visible}{ - logical; set a visibility state}
}
\description{
  Set Spectrino to be visible or hidden, if Visible = TRUE shows Spectrino else hides Spectrino. Gets back the current visibility. If you want to use Spectrino only as a data-tank, that is the way to hide it.
}
\value{
  \code{spnSetVis} Gets back the current visibility.
}
\author{Teodor Krastev}
\examples{
# Initialization of Spectrino
spnNew() 

# shows Spectrino
b1 <- spnSetVis(TRUE)

# hides Spectrino
b1 <- spnSetVis(FALSE) 

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}