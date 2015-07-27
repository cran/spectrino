\name{spnOpenSpc}
\title{Open spec in spec-group}
\usage{
spnOpenSpc(Grp,SFilename)
}
\alias{spnOpenSpc}
\arguments{
\item{Grp}{ - the name(character string) or the index(integer) of the group; 0 - active group.}
\item{SFilename}{ - character string. The path is ignored and the path of current spec-group is assumed, so it would be better if you don't use path}
}
\description{
Open spec in Grp spec-group. All the spec files from one spec-group must be in the direcory of the group file, so avoid using a path for SFilename.
The names of groups or specs are the filenames of the respected ones without path and extension (no space or special characters allowed). To avoid misinterpretation the names of group or spec cannot be numbers (even the operation system let you do it).
}
\value{
  \code{spnOpenSpc} returns returns the number of specs after the adding, which is the index of added spec.
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnOpenGrp} , \link{spnOpenTree}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# delete all the specs from second group;
spnDelSpc(2,"*") 

# that will open existing spec Test23.txt from the directory of "Test2" 
i1 <- spnOpenSpc("Test2","test23.txt") 

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface} 