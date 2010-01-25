\name{freqconv}
\alias{bark2hz}
\alias{hz2bark}
\alias{mel2hz}
\alias{hz2mel}
\title{ Frequency scale conversion }
\description{
Perform frequency scale conversions between Hertz, Bark- and different variants
von the Melscale.
}
\usage{
bark2hz(z)
hz2bark(f)
hz2mel(f, htk=FALSE)
mel2hz(z, htk=FALSE)
}
\arguments{
  \item{f}{ Frequency in Hertz }
  \item{z}{ Frequency in the auditory frequency scale }
  \item{htk}{ Use the HTK-Melscale ('TRUE') or Slaney's Melscale from the
Auditory Toolbox ('FALSE') }
}
\value{
The value of the input in the target frequency scale.
}
\references{Daniel P. W. Ellis:
\url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/}, Malcolm
Slaney: Auditory Toolbox }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mel }
\keyword{ bark }
\keyword{ hertz }
\keyword{ frequency }
\keyword{ conversion }
