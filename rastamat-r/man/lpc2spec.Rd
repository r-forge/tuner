\name{lpc2spec}
\alias{lpc2spec}
\title{ LPC to spectra conversion }
\description{ Convert the LPC coefficients back into spectra. }
\usage{
lpc2spec(lpcas, nout = 17)
}
\arguments{
  \item{lpcas}{ matrix of LPC coefficients for each time frame }
  \item{nout}{ number of freq channels }
}
\value{
  \item{features }{frequence features for each time frame}
  \item{Fout }{}
  \item{Mout }{}
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }
\seealso{ \code{\link{lpc2cep}}, \code{\link{spec2cep}}, \code{\link{cep2spec}} }
\examples{
}
\concept{ lpc }
\concept{ spectra }
\concept{ conversion }
