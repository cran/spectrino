\name{spnChartBlock}
\title{Initialization of block's chart}
\usage{
spnChartBlock(Block, listOfProps)
}
\alias{spnChartBlock}
\arguments{
\item{Block}{ The name of the block whose chart is set}
\item{listOfProps}{ Vector with the property names to be charted}
}
\description{
  Set the properties to be charted with each new iteration. These properies could be set from r with this command or from Spectrino app block interface. 
}
\value{
  \code{spnChartBlock} - Gets back TRUE if opration is successful or FALSE if any of properties does mach block's  properies or the chart is disabled (not enabled).
}
\author{Teodor Krastev}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test group of blocks
spnOpenGroupOfBlocks("<test>")

spnChartBlock("test1",c("objective.0","sug.epsilon"))

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}