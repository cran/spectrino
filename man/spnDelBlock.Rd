\name{spnDelBlock}
\title{Delete a block from the current group of blocks}
\usage{
spnDelBlock(Block)
}
\alias{spnDelBlock}
\arguments{
\item{Block}{ - character string of the name of the block to be deleted. 
-- <ALL> will delete all the block leaving empty group of blocks.
-- <GROUP> will close current group of blocks leaving only the console behind.
}
}
\description{
Delete a block from the current group of blocks. You can delete all blocks <ALL> or the entire group <GROUP>. If you want to open a block, a group of blocks (even empty) must be present.}
\value{
  \code{spnDelBlock} returns TRUE if successful, FALSE - otherwise.
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnOpenBlock} , \link{spnOpenGroupOfBlocks} , \link{spnSaveBlock}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenGroupOfBlocks("<test>")

# delete a block
spnDelBlock("test2")

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}