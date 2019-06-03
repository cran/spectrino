\name{spnGetGrpCount}
\title{Number of groups loaded in the top tab/spec-tree}
\usage{
spnGetGrpCount()
}
\alias{spnGetGrpCount}
\description{
  Counts the number of spec-groups loaded in the top spec-tree.
}
\value{
  \code{spnGetGrpCount} returns number of spec-groups loaded in the top tab/spec-tree. 
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
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}