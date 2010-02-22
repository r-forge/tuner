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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(cep, nfreq=21, type=2){

  ncep <- nrow(cep)
  ncol <- ncol(cep)

  dctm  <- matrix(rep(0, ncep*nfreq), ncol=col)
  idctm <- t(dctm)

  if ((type == 2) || (type == 3)) {
    for (i in 1:ncep) {
      dctm[i,] <- cos((i-1) * seq(1, (2*nfreq-1), 2) / (2*nfreq) * pi) * sqrt(2/nfreq)
    }
    if (type == 2) {
      dctm[1,] <- dctm[1,]/sqrt(2)
    } else {
      dctm[1,] <- dctm[1,]/2
    }
    idctm <- t(Conj(dctm))
  } else if (type == 4) {
    for (i in 1:ncep) {
      idctm[,i] <- 2*cos((i-1) * t(1:nfreq) / (nfreq+1) * pi)
    }
    idctm[, c(1, ncep)] <- idctm[, c(1, ncep)]/2
  } else { 
    for (i in 1:ncep) {
      idctm[,i] <- 2*cos((i-1) * t(0:(nfreq-1)) / (nfreq-1) * pi)
    }
    idctm[, c(1, ncep)] <- idctm[, c(1, ncep)]/2
  }

  spec <- exp(idctm*cep)

  return(list(spec=spec, idctm=idctm))
  }
}
\keyword{ cepstra }
\keyword{ spectra }
\keyword{ conversion }
