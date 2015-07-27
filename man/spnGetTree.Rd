\name{spnGetTree}
\title{Gets specs from all the groups}
\usage{
spnGetTree(OnlyChecked)
}
\alias{spnGetTree}
\arguments{
\item{OnlyChecked}{ - logical; if true gets only the checked specs}
}
\description{
  Get spectra from all the groups. Only the checked ones or all of them. Variables are by columns; measurements are by rows. The reason is "prcomp" principal component analysis accepts that order of data. 
Excluding the spectra from a group called "unknowns". That protected name is supposed to be for testing purposes only, so the data from that group are not included in all-data-get command.
}
\value{
  \code{spnGetTree} returns the matrix of specs from all the groups.
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnGetSpc} , \link{spnGetGrp}, \link{spnGetRefer}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# all the specs from all the groups (excluding "unknowns" group, if any)
m1 <- spnGetTree(FALSE) 

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}