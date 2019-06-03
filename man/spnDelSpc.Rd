\name{spnDelSpc}
\title{Delete a spec from a group in the top tab/spec-tree}
\usage{
spnDelSpc(Grp,Spc)
}
\alias{spnDelSpc}
\arguments{
\item{Grp}{ - the name(character string) or the index(integer) of the group; 0 - active group.}
\item{Spc}{ - the name(character string) or the index(integer) of spec; 0 - selected spec; "*" or "<ALL>" - all specs.}
}
\description{
  Delete Spc spectrum from Grp group in the top tab/spec-tree. If Spc="*" or "<ALL>" then delete all of the spectra in that group them.
}
\value{
  \code{spnDelSpc} The function returns number of the spectra in that group after the deleting \code{spnGetSpcCount(false,Grp)}
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnDelGrp}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# delete third spec from the first group 
# i1 - the number of specs after deleting
i1 <- spnDelSpc(1,3) 

# delete all the specs from the active group;
spnDelSpc(0,"*") 

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}