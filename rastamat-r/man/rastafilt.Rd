\name{rastafilt}
\alias{rastafilt}
\title{Using RASTA filter}
\description{
  \code{rastafilt} ist used to filter each frequency band with the RASTA (Relative Spectral Transform) filter.}
\usage{
  rastafilt(x)
}
\arguments{
  \item{x}{a matrix where rows of \code{x} are the critical bands and columns of \code{x} are the frames.}
}
\details{
}
\value{
  Matrix of the same type as \code{x} after RASTA filtering.
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }
\seealso{{\code{\link{rastaplp}}}
\examples{
  testsound <- normalize(sine(400) + sine(1000) + square(250), "16")
  pspectrum <- powspec(testsound@left, testsound@samp.rate)
  aspectrum <- audspec(pspectrum, testsound@samp.rate)$aspectrum
  nl_aspectrum <- log(aspectrum)
  ras_nl_aspectrum <- rastafilt(nl_aspectrum)
}
\concept{ filter }
\concept{ frequency }
\concept{ RASTA }
