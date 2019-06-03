\name{spnOpenBlock}
\title{Open a block of properties}
\usage{
spnOpenBlock(Block, atPos = -1)
}
\alias{spnOpenBlock}
\arguments{
\item{Block}{ - character string, name of the block to open. All the blocks must be in the same directory as their group of block file.}
\item{atPos}{ - integer (0..8) position of the block 1..8 are positions from top to bottom left to right (see Spectrino option for the map). 0 if you wish the block to be hidden. -1 is for automatic placement.
}}
\description{
Open a block of properties at a position. You can specify the whole file name including *.blk extention, but it's a bit pointless as all the blocks must be in the same directory as their group of block file. }
\value{
  \code{spnOpenBlock} returns TRUE if successful, FALSE - otherwise. 
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnOpenGroupOfBlocks} , \link{spnSaveBlock}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenGroupOfBlocks("<test>")

# delete a block
spnDelBlock("test2")

# open a block
spnOpenBlock("test2")

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}