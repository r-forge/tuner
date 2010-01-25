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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, modelorder = 8) 
{
    nbands <- nrow(x)
    px <- planFFT(2 * nbands - 2)
    r <- apply(rbind(x, x[seq(nbands - 1, 2, -1), ]), 2, function(y) Re(IFFT(y, 
        plan = px)))
    r <- r[1:nbands, ]
    levcoef <- levinson(r, modelorder)
    y <- t(levcoef$a)/matrix(rep(levcoef$v, modelorder + 1), 
        nrow = modelorder + 1, byrow = TRUE)
    return(y)
  }
}
\keyword{ lpc }
\keyword{ plp }
\keyword{ levinson }
\keyword{ durbin }
\keyqord{ recursion }
