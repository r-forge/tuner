\name{lifter}
\alias{lifter}
\title{ Liftering of cepstra }
\description{
   Apply liftering to a matrix of cepstra.
}
\usage{
lifter(x, lift = 0.6, inv = FALSE)
}
\arguments{
  \item{x}{ Matrix of cepstra, one sample/time frame per column. }
  \item{lift}{ Liftering exponent, positive real number or a negative integer. }
  \item{inv}{ Invert the liftering (undo a previous liftering). }
}
\details{
 If 'lift' is positive, then perform \eqn{x i^lift}, \eqn{i = 1, ldots, nrow(x)}
liftering. If 'lift' is a negative integer, then perform HTK-style sin-curve
liftering with length 'lift'.
}
\value{
Matrix of the liftered cepstra.
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\examples{
}
\concept{ liftering }
