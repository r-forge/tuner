\name{cep2spec}
\alias{cep2spec}
\title{ Cepstra to Spectra Conversion }
\description{
  Reverse the cepstrum to recover a spectrum. I.e. converse of \code{\link{spec2cep}}.
}
\usage{
cep2spec(cep, nfreq = 21, type = 2)
}
\arguments{
  \item{cep}  { Input cepstra }
  \item{nfreq}{ How many points to reconstruct in spec }
  \item{type} { DCT Type }
}
\value{
  \item{spec} {Matrix of resulting spectra.}
  \item{idctm}{Inverse DCT matrix that spec was multiplied by to give spec.}
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{
%%  ~~who you are~~
}
\seealso{ \code{\link{spec2cep}} }
\examples{
}
\concept{cepstra}
\concept{spectra}
\concept{conversion}
