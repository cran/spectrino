\name{spnGetTreeNames}
\title{Get a vector of tree names or filenames}
\usage{
spnGetTreeNames(Filenames = FALSE)
}
\alias{spnGetTreeNames}
\arguments{
\item{Filenames}{ - the boolean indicates if all Filenames will be returned or just the names.} 
}
\description{
  Get a vector of tree names or filenames depending on Filenames FALSE/TRUE respectively
}
\value{
  \code{spnGetTreeNames} a vector of tree names or filenames.
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

# the filesname of all trees
s1 <- spnGetTreeNames(TRUE) 

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}