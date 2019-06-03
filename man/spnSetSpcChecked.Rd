\name{spnSetSpcChecked}
\title{Set checked state of spec(s) in the top tab/spec-tree}
\usage{
spnSetSpcChecked(Grp,Spc,Checked)
}
\alias{spnSetSpcChecked}
\arguments{
\item{Grp}{ - the name(character string) or the index(integer) of the group; 0 - active group; "*" - all groups}
\item{Spc}{ - the name(character string) or the index(integer) of spec; 0 - selected spec; "*" - all specs.}
\item{Checked}{ - logical, the state which will be set}
}
\description{
Set Spc spectrum of Grp spec-group checkbox(es) to checked/unchecked state. 
If Spc="*" then all of specs in Grp group are set. If Grp="*" then all of spectra in all groups are set to Checked (in that case Spc is ignored).
}
\value{
  \code{spnSetSpcChecked} none
}
\author{Teodor Krastev}
\seealso{
\code{\link{spnGetSpcChecked}}, \code{\link{spnActGrp}}
}
\examples{ 
# Initialization of Spectrino
spnNew() 

# generate test set
spnOpenTree("<test>")

# check just one
spnSetSpcChecked(2,3,TRUE)

# all the spec from second group to OFF 
spnSetSpcChecked(2,"*",FALSE)

# all the specs in all groups to ON
spnSetSpcChecked("*","*",TRUE)

# Release of Spectrino
#spnFree(TRUE)
}
\keyword{programming}\keyword{interface}