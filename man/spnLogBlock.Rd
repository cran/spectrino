\name{spnLogBlock}
\title{Log to a blocks log or in the console}
\usage{
spnLogBlock(Block, text)
}
\alias{spnLogBlock}
\arguments{
\item{Block}{ - the name(character string) of the target block; if cannot be found (e.g. "") the main log (console) is used.}
\item{text}{ - message character string to be logged. Usually it's a some text message, but there are some special cases:
 -- <property.name> wil log the name and the value of that property
 -- <ALL> will log all the properties (name=value) of that block 
 -- <CLEAR> will clear the log
}
}
\description{
Log some text and/or values in the blocks log or in the console. The user can send some text, or use some special syntax for listing one or all properties. Alernative to that command you can use a log template (set from block's menu) which contains text and $property.name$ the last one will be replaced by that property value. The template will ber used at each iteration.
}
\value{
  \code{spnLogBlock} returns TRUE if successful, FALSE - otherwise.
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnChartBlock} , \link{spnIteration}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# write into the console
spnLogBlock("","some text")

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}