\name{spnOpenGrp}
\title{Opens/Creates a spec-group}
\usage{
spnOpenGrp(GFilename,NewGrp)
}
\alias{spnOpenGrp}
\arguments{
\item{GFilename}{ - character string; If the path is missing, the path of current spec-tree is assumed. The extension (*.SGR) is not a must. 
All the filenames or directories must be with forward slashes e.g. D:/Prime/Data/Test.sgr}
\item{NewGrp}{ - logical. If NewGrp is true then GFilename shouldn't exist to be created. If NewGrp is false then GFilename must exist to be opened.}
}
\description{
  Open/Create a spec-group. All the spec files from one spec-group must be in the direcory of the group file.
The names of spec-groups or specs are the filenames of the respected ones without path and extension (no space or special characters allowed). To avoid misinterpretation the names of group or spec cannot be numbers (even the operation system let you do it).
}
\value{
  \code{spnOpenGrp} returns the number of groups after the adding, which is the index of added group. \code{spnGetGrpCount}

}
\author{Teodor Krastev}
\seealso{
\code{\link{spnOpenSpc} , \link{spnOpenTree} , \link{spnSaveGrp}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# empty the whole list of groups
spnDelGrp("*") 

# creates a new group; 
# the directory must exists and the spec files must be in it
# i1 <- spnOpenGrp("D:/Prime/Data/Oils.sgr", TRUE) 

# opens "Test2.sgr" file from current spec-tree directory
i2 <- spnOpenGrp("Test2", FALSE) 

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}