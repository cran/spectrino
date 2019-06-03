\name{spnGetSpcCount}
\title{Number of spectra in Grp spec-group in the top tab/spec-tree}
\usage{
spnGetSpcCount(OnlyChecked,Grp)
}
\alias{spnGetSpcCount}
\arguments{
\item{OnlyChecked}{ - logical; if true gets only the checked specs}
\item{Grp}{ - the name(character string) or the index(integer) of the spec-group; 0 - active group.}
}
\description{
  Counts the number of specs in Grp group in the top tab/spec-tree.
}
\value{
  \code{spnGetSpcCount} returns number of specs in Grp group in the top tab/spec-tree.
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnGetGrpCount} }
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# the number of the checked specs in second group
i1 <- spnGetSpcCount(TRUE,2) 

# the number of specs in the active group
i2 <- spnGetSpcCount(FALSE,0)

# the number of specs in "Test3" group
i3 <- spnGetSpcCount(FALSE,"Test3")

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}