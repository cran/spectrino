\name{spnInstallApp}
\title{Install Spectrino application from local zip file or spectrino website}
\usage{
spnInstallApp(zipFile="")
}
\alias{spnInstallApp}
\arguments{
\item{zipFile}{ - character string to local zip file contains exec directory of spectrino application.  }
}
\description{
Install spectrino application, which is Windows application required by the spectrino package to be functional. 
If zipFile argument is supplied spnInstallApp will install it from there, if left empty (default) it will download it from spectrino website and install it.
This function is supposed to be used only once. The function spnNew() checks of spectrino.exe presence and if absent it will offer you to run spnInstallApp for you.
If you run spnInstallApp with spectrino application already installed, the function will offer you to overwrite it.}
\value{
  \code{spnInstallApp} returns TRUE if succesful and FALSE otherwise.
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnNew} , \link{spnFree} }
}
\examples{
# Download and install the last version of Spectrino application
spnInstallApp(NULL)

# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}