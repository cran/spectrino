\name{spnIsError}
\title{Check for spectrino error in a function result}
\usage{
spnIsError(rslt)
}
\alias{spnIsError}
\arguments{
\item{rslt}{ check if rslt is character and if it is, if it is an error message (starts with Error:)}
}
\description{
  Checking if the result of spectrino function is error. In your code, it's a good practice to check 
the result from any spectrino function (except for spnNew, spnFree and spnCheck). 
From command line there is no sense to call this function because you would see the error message on the terminal.
}
\value{
  \code{spnIsError} returns logical for spectrino error. 
}
\author{Teodor Krastev}
\examples{
spnNew() 

# generate test set
spnOpenTree("<test>")

# all the specs from all the groups (excluding "unknowns" group, if any)
m1 <- spnGetTree(FALSE) 

# is there an error 
spnIsError(m1)

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}