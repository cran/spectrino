\name{spnSaveBlock}
\title{Save a block of properties}
\usage{
spnSaveBlock(Block)
}
\alias{spnSaveBlock}
\arguments{
\item{Block}{ - the name(character string) of block to be saved. The destination directory is that of the group of block the block belongs to.
-- <ALL> will save all the blocks from current group of blocks}
}
\description{
Save a block of properties in the folder of the group of blocks it belongs to.
}
\value{
  \code{spnSaveBlock} returns TRUE if successful, FALSE - otherwise.
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnOpenBlock} , \link{spnSaveGroupOfBlocks}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# generate test set
spnOpenGroupOfBlocks("<test>")

# save a block
spnSaveBlock("test3")

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}