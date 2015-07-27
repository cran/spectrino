\name{spnSetPPOpt}
\title{Set pre-processing options of Spectrino}
\usage{
spnSetPPOpt(OptionList)
}
\alias{spnSetPPOpt}
\arguments{
\item{OptionList}{ - string; comma separated list of options as they are in Preprocess section of a spec-tree. 
}
}
\description{Set Spectrino pre-processing options as string of semicolumn delimited list
of following options (see example)
Baseline=<integer>
BaselineOn=0/1
MassBins=0/1
Normalize=0/1
MeanExtract=0/1
BaseGrp=<GroupName>
LowLimit=<integer>
HighLimit=<integer>
Precision=<integer 1..10>
}
\value{
  \code{spnSetPPOpt} Gets back the full options list.
}
\author{Teodor Krastev}
\examples{
# Initialization of Spectrino
spnNew() 

# Set Spectrino pre-processing options
spnSetPPOpt("Normalize=0;MeanExtract=0") 

# Release of Spectrino
spnFree(TRUE)
}
\keyword{programming}\keyword{interface}