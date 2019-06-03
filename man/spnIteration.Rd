\name{spnIteration}
\title{Iteration control in blocks}
\usage{
spnIteration(Initial = 0)
}
\alias{spnIteration}
\arguments{
\item{Initial}{ 
if Initial <> 0 the iteration counter is est 0
-2: only internal count (no progress bar)
-1: normal count including progress bar, when the final count is unknown
 0: one iteration (moves the iteration count 1 up)
 n: (n>0) the expected number of iterations, with progress bar
}}
\description{
  Control iteration count: 
  - if the expected number of iterations is unknown Initial should be set to -2 or -1 (without or with progres bar) 
  - if the expected number of iterations is known Initial should be set to that number
  
after initializing the iteration counter with any Initial<>0, your code should call Iteration() on each iteration (usually at the end)

Apart of the progress bar, the block will react to an iteration depending of how have been set. E.g. Chart will draw next value(s) and/or log will write next status according to the log template.
}
\value{
  \code{spnIteration} - Gets back the iteration counter value.
}
\author{Teodor Krastev}
\examples{
# Initialization of Spectrino
spnNew() 

# initializing with 5 iteration expected iteration count
spnIteration(5)

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}