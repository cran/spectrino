\name{spnAddTree}
\title{Open a spec-tree in a new tab}
\usage{
spnAddTree(TFilename,InclOpt)
}
\alias{spnAddTree}
\arguments{
\item{TFilename}{ - character string. The extension (*.STR) is not a must. All the filenames or directories must be with forward slashes e.g. D:/Spectrino/Data/Test.str. "<test>" string generates test example (simulation)}
\item{InclOpt}{ - integer (0,1,2); InclOpt rules where the preprocessing options will be taken from. 
If InclOpt = 0 then factory setting (no preprocessing) is used; 1 is for last used one; if 2 - the options are taken from TFilename}
}
\description{
Open a spec-tree in a new tab from TFilename. If the active tab/spec-tree is empty (<no-name> title and no groups opened) that tab will be used instead of creating a new one. The spec-tree file (.str) always is saved with the preprocessing options. If the file does not exist the function will create an empty one, so you can add groups and specs and save it with spnSaveTree("") }
\value{
  \code{spnAddTree} returns the number of tabs/spec-trees. \code{spnGetGrpCount}
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnOpenTree} , \link{spnOpenSpc} , \link{spnOpenGrp} , \link{spnSaveTree}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnAddTree("<test>")

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}