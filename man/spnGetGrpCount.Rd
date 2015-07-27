\name{spnGetGrpCount}
\title{Number of groups loaded}
\usage{
spnGetGrpCount()
}
\alias{spnGetGrpCount}
\description{
  Counts the number of spec-groups loaded in the current spec-tree.
}
\value{
  \code{spnGetGrpCount} returns number of spec-groups loaded. 
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnGetSpcCount} }
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

n <- spnGetGrpCount()

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}