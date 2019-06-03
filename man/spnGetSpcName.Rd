\name{spnGetSpcName}
\title{Gets the spec name with an index SpcIdx from Grp group}
\usage{
spnGetSpcName(Grp,SpcIdx)
}
\alias{spnGetSpcName}
\arguments{
\item{Grp}{ - the name(character string) or the index(integer) of the spec-group; 0 - active group.}
\item{SpcIdx}{ - the index(integer) of the spec; 0 - selected spec. If SpcIdx="*" gets back a comma-separated list of all specs names in the group.} 
}
\description{
  Gets the spec name with an index SpcIdx from Grp group in the top tab/spec-tree
}
\value{
  \code{spnGetSpcCount} returns the name(character string) of spec with SpcIdx index from Grp group.
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnGetGrpName} }
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# the name of second spec from the first group
s1 <- spnGetSpcName(2,1) 

# the names-list of the active group
s2 <- spnGetSpcName(0,"*")

# the name of the third spec from "Test2" group
s3 <- spnGetSpcName("Test2",3)

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}