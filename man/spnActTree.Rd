\name{spnActTree}
\title{Get/Set active tree (the top tab)}
\usage{
spnActTree(Tree)
}
\alias{spnActTree}
\arguments{
\item{Tree}{ - the name(character string) or the index(integer) of the tree; if 0 - just gets the active tree index.}
}
\description{
  Get/Set active tree to Tree by name or index. if Tree=0 only get active tree index; else set one
}
\value{
  \code{spnActTree} returns the index (1-based index) of the active tree. 
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnActGrp}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# set third group to be active
spnActTree(1)

# get the number of active group
i <- spnActGrp(0) 

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}