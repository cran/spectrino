\name{spnGetGrpName}
\title{Get the group name by index in the top tab/spec-tree}
\usage{
spnGetGrpName(GrpIdx)
}
\alias{spnGetGrpName}
\arguments{
\item{GrpIdx}{ - the index(integer) of the spec-group in the top tab/spec-tree. Use GrpIdx=0 for active group. If GrpIdx="*" or "<ALL>" gets back a comma-separated list of all groups names.} 
}
\description{
  Get the group name with an index GrpIdx in the top tab/spec-tree
}
\value{
  \code{spnGetSpcCount} returns name(character string) of spec-group with GrpIdx index.
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnGetSpcName} }
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# the name of second spec-group
s1 <- spnGetGrpName(2) 

# the name of the active group
s2 <- spnGetGrpName(0)

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}