\name{spnSaveTree}
\title{Save current spec-tree with the preprocessing options.}
\usage{
spnSaveTree(TFilename)
}
\alias{spnSaveTree}
\arguments{
\item{TFilename}{ - character string. If TFilename is empty then Spectrino uses the proper name of the spec-tree.}
}
\description{
Save the current spec-tree along with the preprocessing options.
}
\value{
  \code{spnSaveTree} returns the full name of saved spec-tree
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnOpenTree} , \link{spnSaveGrp}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# save the spec-tree under its name
spnSaveTree("") 

# rename  the spec-tree and save it 
spnSaveTree("savenow")

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}