\name{spnOpenTree}
\title{Open a spec-tree}
\usage{
spnOpenTree(TFilename,InclOpt)
}
\alias{spnOpenTree}
\arguments{
\item{TFilename}{ - character string. The extension (*.STR) is not a must. All the filenames or directories must be with forward slashes e.g. D:/Spectrino/Data/Test.str. "<test>" string generates test example}
\item{InclOpt}{ - integer (0,1,2); InclOpt rules where the preprocessing options will be taken from. 
If InclOpt = 0 then factory setting (no preprocessing) is used; 1 is for last used one; if 2 - the options are taken from TFilename}
}
\description{
Open a spec-tree from TFilename. The spec-tree file (.str) always is saved with the preprocessing options. The function returns 
To create new spec-tree you have to empty current one by \code{spnDelGrp("*")} and save the empty one under new name.}
\value{
  \code{spnOpenTree} returns the number of groups in the new spec-tree. \code{spnGetGrpCount}
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnOpenSpc} , \link{spnOpenGrp} , \link{spnSaveTree}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}