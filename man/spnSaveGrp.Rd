\name{spnSaveGrp}
\title{Save a spec-group in the top tab/spec-tree}
\usage{
spnSaveGrp(Grp,GFilename)
}
\alias{spnSaveGrp}
\arguments{
\item{Grp}{ - the name(character string) or the index(integer) of the group; 0 - active group; "*" - all groups. If Grp="*" then all groups are saved under their proper names (in that case GFilename is ignored, but some empty string  has to be provided) and nothing gets back to R.}
\item{GFilename}{ - character string. The path of GFilename of is ignored, because any group file must be in the same directory as the specs in it. If GFilename is empty (the most common use), then Spectrino uses the proper name of the group.}
}
\description{
Save Grp group as GFilename file. The most common use is with GFilename="", to save the group under its proper name.
}
\value{
  \code{spnSaveGrp} returns the full name of saved spec-group, except for Grp="*".
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnOpenGrp} , \link{spnSaveTree}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# save second group under its name;
s1 <- spnSaveGrp(2,"") 

# rename "Test2" group to "gassew" and then save it;
s2 <- spnSaveGrp("Test2","gassew")

# save all the groups under their proper names;
spnSaveGrp("*","")

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}