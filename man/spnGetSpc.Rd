\name{spnGetSpc}
\title{Get the vector of specific spec in the top tab/spec-tree }
\usage{
spnGetSpc(Grp,Spc)
}
\alias{spnGetSpc}
\arguments{
\item{Grp}{ - the name(character string) or the index(integer) of the spec-group; 0 - active group.}
\item{Spc}{ - the name(character string) or the index(integer) of spec; 0 - selected spec; "*" or "<ALL>" - all specs}
}
\description{
  Get one spectrum (vector) - only the Y-values of raw (unprocessed) data. All the spectra in a group  are assumed to have common X set of values, so if there is loaded spectrum in different X values, the spectrum is recalculated to fit that reference set. If Spc is * the command is equivalent to getGetGrp(False,Grp) and gives back preporocessed data.
}
\value{
  \code{spnGetSpc} returns one spectrum (vector) - only the Y-values of raw (unprocessed) data. 
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnGetGrp} , \link{spnGetTree} , \link{spnGetRefer}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# if "Test2" is the second group, and "test23" - the third spec in it
v1 <- spnGetSpc(2,3) 
# is equivalent to 
v1 <- spnGetSpc("Test2","test23")

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}