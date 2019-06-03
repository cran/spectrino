\name{spnGetGrp}
\title{Get a group data in the top tab/spec-tree}
\usage{
spnGetGrp(OnlyChecked,Grp)
}
\alias{spnGetGrp}
\arguments{
\item{OnlyChecked}{ - logical; if true gets only the checked specs.}
\item{Grp}{ - the name(character string) or the index(integer) of a group in the top tab/spec-tree; 0 - active group.}
}
\description{
  Get spectra from one spec-group (matrix) in the top tab/spec-tree. All the spectra in a group  are assumed to have common X set of values, so if there is loaded spectrum in different X values, the spectrum is recalculated to fit that reference set. 
}
\value{
  \code{spnGetGrp} returns a preprocessed group data in matrix. Spectra are always in rows (one spectrum is one row). The variables are columns, one variable (e.g. mass) is one column. 
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnGetSpc} , \link{spnGetTree} , \link{spnGetRefer}}
}
\examples{
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# all the checked specs from the first group
m1 <- spnGetGrp(TRUE,1)

# all the specs from "Test2" group
m2 <- spnGetGrp(FALSE,"Test2") 

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}