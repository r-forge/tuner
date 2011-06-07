\name{lpc2spec}
\alias{lpc2spec}
\title{ LPC to spectra conversion }
\description{ Convert the LPC coefficients back into spectra. }
\usage{
lpc2spec(lpcas, nout = 17)
}
\arguments{
  \item{lpcas}{ matrix of LPC coefficients (one column for each time frame) }
  \item{nout}{ number of freq channels }
}
\value{
  \item{features }{ Frequency features for each time frame}
  \item{Fout }{ Argument of frequencies in complex plane }
  \item{Mout }{ Magnitude of frequencies in complex plane }
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }
\seealso{ \code{\link{lpc2cep}}, \code{\link{spec2cep}}, \code{\link{cep2spec}} }
\examples{
  testsound <- normalize(sine(400) + sine(1000) + square(250), "16")
  pspectrum <- powspec(testsound@left, testsound@samp.rate)
  aspectrum <- audspec(pspectrum, testsound@samp.rate)$aspectrum
  lpcas <- dolpc(aspectrum, 10)
  spec <- lpc2spec(lpcas)
}
\concept{ lpc }
\concept{ spectra }
\concept{ conversion }
