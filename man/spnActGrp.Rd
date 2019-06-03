\name{spnActGrp}
\title{Get/Set active group in the top tab}
\usage{
spnActGrp(Grp)
}
\alias{spnActGrp}
\arguments{
\item{Grp}{ - the name(character string) or the index(integer) of the group; if 0 - just gets the active group index.}
}
\description{
  Get/Set active group to Grp in the top tab. if Grp=0 only get active group index; else set one
}
\value{
  \code{spnActGrp} returns the index (1-based index) of the active group. 
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnSetSpcChecked}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# get the number of active group
i <- spnActGrp(0) 

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}