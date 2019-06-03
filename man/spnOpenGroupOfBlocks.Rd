\name{spnOpenGroupOfBlocks}
\title{Open a group of blocks}
\usage{
spnOpenGroupOfBlocks(Filename)
}
\alias{spnOpenGroupOfBlocks}
\arguments{
\item{Filename}{ - character string. The extension (*.GBK) is not a must. If the path is missing the default (blocks) folder is assumed.
All the filenames or directories must be with forward slashes e.g. D:/Spectrino/Blocks/Test.gbk. "<test>" string generates test example group of blocks.}
}
\description{
Open a group of blocks from file "Filename" as list of names of blocks and then it opens all the blocks from the list. "<test>" as filename will generate test example group.}
\value{
  \code{spnOpenGroupOfBlocks} returns TRUE if successful, FALSE - otherwise.
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnOpenBlock} , \link{spnSaveBlock} , \link{spnSaveGroupOfBlocks}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenGroupOfBlocks("<test>")

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}