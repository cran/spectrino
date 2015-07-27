\name{spnActGrp}
\title{Get/Set active group}
\usage{
spnActGrp(Grp)
}
\alias{spnActGrp}
\arguments{
\item{Grp}{ - the name(character string) or the index(integer) of the group; if 0 - just gets the active group number.}
}
\description{
  Get/Set active group to Grp. if Grp=0 only get active group; else set one
}
\value{
  \code{spnActGrp} returns the number of the active group. 
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnSetSpcChecked}}
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

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}