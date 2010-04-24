\name{rastaplp}
\alias{rastaplp}
\title{RASTA}
\description{ log rasta with fixed parameters }
\usage{
rastaplp(samples, sr = 8000, dorasta = 1, modelorder = 8)
}
\arguments{
  \item{samples}{ Vector of samples.}
  \item{sr}{ Sampling rate of samples.}
  \item{dorasta}{}
  \item{modelorder}{}
}
\value{
  \item{cepstra }{Matrix of resulting cepstra.}
  \item{spectra }{Matrix of resulting spectra.}
  \item{pspectrum }{Matrix of resulting power spectrum.}
  \item{lpcas }{Matrix of the normalized AR coefficients.}
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }
\seealso{ \code{\link{powspec}}, \code{\link{audpec}}, \code{\link{aspectrum}}, \code{\link{rastafilt}}, \code{\link{dolpc}}, \code{\link{lpc2spec}} }
\examples{
}
\concept{ spectrum }
\concept{ rasta }
\concept{ cepstrum }
