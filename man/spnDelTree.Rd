\name{spnDelTree}
\title{Delete a tree / tab }
\usage{
spnDelTree(Tree)
}
\alias{spnDelTree}
\arguments{
\item{Tree}{ - the name(character string) or the index(integer) of the tree; 0 - active tree; "<ALL>" - all trees.}
}
\description{
  Delete a tree(s) from spec side. If Tree="<ALL>" then delete all of the trees.
}
\value{
  \code{spnDelTree} returns the number of trees after the deleting. 
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

# delete active tree / tab
spnDelTree(0)

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}