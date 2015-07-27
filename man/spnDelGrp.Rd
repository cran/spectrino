\name{spnDelGrp}
\title{Delete a group(s)}
\usage{
spnDelGrp(Grp)
}
\alias{spnDelGrp}
\arguments{
\item{Grp}{ - the name(character string) or the index(integer) of the group; 0 - active group; "*" - all groups.}
}
\description{
  Delete Grp group. If Grp="*" then delete all of the groups from the spec-tree.
}
\value{
  \code{spnDelGrp} returns the number of groups after the deleting. \code{(spnGetGrpCount)}
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnDelSpc}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# delete third group from the list
spnDelGrp(3)

# empty the whole list of groups
spnDelGrp("*") 

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}