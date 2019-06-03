\name{spnGetBlockNames}
\title{Names of all properies of a block or all the blocks names}
\usage{
spnGetBlockNames(Block = "")
}
\alias{spnGetBlockNames}
\arguments{
\item{Block}{ - the name(character string) of a block; if empty - gets back the vector of block names.}
}
\description{
  Get the names of all properies of a block or all the blocks names in the current group of blocks
}
\value{
  \code{spnGetBlockNames} returns a vector with the names of all properies of a block or all the blocks names. 
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnGetGrpName}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenGroupOfBlocks("<test>")

# all the properties of block test1
spnGetBlockNames("test1")

# all the blocks if current group of blocks
spnGetBlockNames() 

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}