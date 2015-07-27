\name{spnNew}
\title{Initialization of Spectrino}
\usage{
spnNew(TimeOut = 2, Host = "127.0.0.1", Port = 9876)
}
\alias{spnNew}
\arguments{
\item{TimeOut}{ time to wait [s] for spectrino application to reply, default is 2. In case of time-out error you may try to increase it}
\item{Host}{ Host IP address for the websocket server, the default is 127.0.0.1 which is localhost}
\item{Port}{ Port of web-socket communication, default is 9876 }
}
\description{
  Check if R-object of Spectrino exists, and if not, creates/initializes Spectrino object/application.
  The command recommendable, but optional - it will be called, when any command is executed, if the R-object of Spectrino does not exists.
}
\value{
  \code{spnNew} - Gets back TRUE if the Spectrino object exists or has been created; otherwise - FALSE.
}
\author{Teodor Krastev}
\examples{
# Initialization of Spectrino
spnNew() 

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}