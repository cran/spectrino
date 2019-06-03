\name{spnInstallApp}
\title{Install Spectrino application from spectrino website or local zip file}
\usage{
spnInstallApp(zipFile="")
}
\alias{spnInstallApp}
\arguments{
\item{zipFile}{ - character string to local zip file contains exec directory of spectrino application. If NULL, the instalation is void. (omitted) }
}
\description{
Install spectrino application, which is Windows application required by the spectrino package to be functional. 
If zipFile argument is supplied spnInstallApp will install it from there, if left empty (default) it will download it from spectrino website and install it.
This function is supposed to be used only once. The function spnNew() checks of spectrino.exe presence and if absent it will offer you to run spnInstallApp for you.
If you run spnInstallApp with spectrino application already installed, the function will offer you to overwrite it. In case of update (when you run R-package version lower than your spectrino app version) that would be the easiest way to update your app.
N.B. Version 2.0.* of spectrino application is fully backwards compatible to 1.5 and 1.6 versions of spectrino R-package.
}
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

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}