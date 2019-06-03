\name{spnGetSpcChecked}
\title{Gets the vector of the state of spec checking boxes of Grp group in the top tab/spec-tree}
\usage{
spnGetSpcChecked(Grp)
}
\alias{spnGetSpcChecked}
\arguments{
\item{Grp}{ - the name(character string) or the index(integer) of the group; 0 - active group.}
}
\description{
  Gets the logical vector of the state of checking boxes of Grp group. 
}
\value{
  \code{spnGetSpcChecked} returns the logical vector of checked spec state. 
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnSetSpcChecked} }
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# the logical vector of checked spec of the first group
bv1 <- spnGetSpcChecked(1) 

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}