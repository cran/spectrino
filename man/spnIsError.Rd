\name{spnIsError}
\title{Check for spectrino error in a function result}
\usage{
spnIsError(rslt)
}
\alias{spnIsError}
\arguments{
\item{rslt}{ check if rslt is character and if it is, if it is an error message (first char is E}
}
\description{
  Check if the result of spectrino function is error. It's a good practice to check 
the result from any spectrino function (except for spnNew, spnFree and spnCheck).
}
\value{
  \code{spnIsError} returns logical for spectrino error. 
}
\author{Teodor Krastev}
\examples{

# some data
mx <- c(10.4, 5.6, 3.1, 6.4, 21.7)

# is there an error 
spnIsError(mx)

er <- "Error: test mistake"

# is there an error 
spnIsError(er)

}
\keyword{programming}\keyword{interface}