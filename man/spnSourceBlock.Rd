\name{spnSourceBlock}
\title{Get the source from a block and optionally execute it}
\usage{
spnSourceBlock(Block, srcIdx, Eval = TRUE)
}
\alias{spnSourceBlock}
\arguments{
\item{Block}{ - character string, a name of a block. }
\item{srcIdx}{ - integer (1,2,3); Gets a source (code) from srcIdx tab of the respective block}
\item{Eval}{ - execute the code in namespace/enviroment you are calling the function from.}
}
\description{
Get the srcIdx source from a block and optionally execute it namespace/enviroment you are calling the function from.}
\value{
  \code{spnOpenTree} returns the number of groups in the new spec-tree. \code{spnGetGrpCount}
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnLogBlock} , \link{spnChartBlock} }
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenGroupOfBlocks("<test>")

# gets source from tab 1 source of the block and execute it
spnSourceBlock("test3", 1, TRUE)

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}