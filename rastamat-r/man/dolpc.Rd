\name{dolpc}
\alias{dolpc}
\title{ (Perceptive) Linear Prediction }
\description{
Compute autoregressive model from spectral magnitude samples via Levinson-Durbin
recursion.
}
\usage{
dolpc(x, modelorder = 8)
}
\arguments{
  \item{x}{ Matrix of spectral magnitude samples (each sample/time frame in one
column). }
  \item{modelorder}{ Lag of the AR model. }
}
\value{
Returns a matrix of the normalized AR coefficients (depending on the input
spectrum: LPC or PLP coefficients). Every column represents one time frame.
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{ \code{\link{levinson}} }
\examples{
}
\concept{ lpc }
\concept{ plp }
\concept{ levinson }
\concept{ durbin }
\concept{ recursion }
