\name{spnSaveGroupOfBlocks}
\title{Save the current Group of Blocks}
\usage{
spnSaveGroupOfBlocks(Filename = "")
}
\alias{spnSaveGroupOfBlocks}
\arguments{
\item{Filename}{ - character string. The extension (*.GBK) is not a must. All the filenames or directories must be with forward slashes e.g. D:/Spectrino/Blocks/Test.GBK. If the path is missing the default block path is assumed. If the argument Filename is missing, the filename when it was opened is assumed.}
}
\description{
Save the current group of blocks as a list of names. Depending of the option from option dialog the command will save (default) or not the block themselves.
If you want to make sure that the blocks are saved regardless that option use \code{spnSaveBlock("<ALL>")} after with this command. Blocks always are saved in the same directory of their group of blocks file.
}
\value{
  \code{spnSaverGroupOfBlocks} returns TRUE if successful, FALSE - otherwise.
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

# save test group
spnSaveGroupOfBlocks("")

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}