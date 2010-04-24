\name{spec2cep}
\alias{spec2cep}
\title{ Spectra to Cepstra Conversion }
\description{
Calculate cepstra from spectral samples (in columns of spec) through Discrete
Cosine Transformation.
}
\usage{
spec2cep(spec, ncep = 13, type = c("t2", "t1", "t3", "t4"))
}
\arguments{
  \item{spec}{ Input spectra (samples/time frames in columns)}
  \item{ncep}{ Number of cepstra to return }
  \item{type}{ DCT Type }
}
\value{
  \item{cep }{Matrix of resulting cepstra.}
  \item{dctm }{Returns the DCT matrix that spec was multiplied by to give cep.}
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{ \code{\link{cep2spec}}, \code{\link{lpc2cep}}, \code{\link{lpc2spec}}
}
\examples{
}
\concept{ cepstra }
\concept{ spectra }
\concept{ conversion }
