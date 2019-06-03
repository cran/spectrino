\name{spnGetProperty}
\title{Get a property value from a block}
\usage{
spnGetProperty(Block, Prop)
}
\alias{spnGetProperty}
\arguments{
\item{Block}{ - the name(character string) of a block.}

\item{Prop}{ - the name(character string) of a property. A special integrated property <ITERS> will give the current number of the iteration counter }
}
\description{
  Get a property value from a block. The properties of a block can be configured from the block menu (configure).
}
\value{
  \code{spnGetProperty} returns the value of the property, the type depends of the type of propery in the block. 
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnSetProperty}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenGroupOfBlocks("<test>")

# set a property
spnSetProperty("test1","objective.0",3.58)

# get a property
spnGetProperty("test1","objective.0") 

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}