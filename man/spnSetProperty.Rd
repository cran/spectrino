\name{spnSetProperty}
\title{Set a property value in a block}
\usage{
spnSetProperty(Block, Prop, Value)
}
\alias{spnSetProperty}
\arguments{
\item{Block}{ - the name(character string) of a block.}

\item{Prop}{ - the name(character string) of a property. }

\item{Value}{ - the value of the property, the type depends of the type of property in the block. }
}
\description{
  Set a property value in a block. The properties of a block can be configured from the block menu (configure).
}
\value{
  \code{spnSetProperty} returns TRUE if successful, FALSE - otherwise.
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnGetProperty}}
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