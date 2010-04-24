\name{lpc2cep}
\alias{lpc2cep}
\title{ LPC to cepstra conversion }
\description{
 Convert the LPC coefficients in each column of \code{a} into frames of cepstra.
}
\usage{
lpc2cep(a, nout = nrow(a))
}
\arguments{
  \item{a}{ Matrix of LPC coefficients. }
  \item{nout}{ Number of cepstra to produce. }
}
\value{
Matrix of cepstra.
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{ \code{\link{lpc2spec}}, \code{\link{spec2cep}}, \code{\link{cep2spec}} }
\examples{
}
\concept{ lpc }
\concept{ cepstra }
\concept{ conversion }
