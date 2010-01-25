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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (spec, ncep = 13, type = c("t2", "t1", "t3", "t4")) 
{
    dctm23 <- function(spec, ncep) {
        srow <- nrow(spec)
        return(cos((seq(ncep) - 1) * matrix(rep(seq(1, (2 * srow - 
            1), 2), each = ncep), nrow = ncep)/(2 * srow) * pi) * 
            sqrt(2/srow))
    }
    dctm2 <- function(spec, ncep) {
        dctm <- dctm23(spec, ncep)
        dctm[1, ] <- dctm[1, ]/sqrt(2)
        return(dctm)
    }
    dctm4 <- function(spec, ncep) {
        srow <- nrow(spec)
        dctm <- cos((seq(ncep) - 1) * matrix(rep(seq(srow), each = ncep), 
            nrow = ncep)/(srow + 1) * pi) * 2
        dctm[, 1] <- dctm[, 1] + 1
        dctm[, srow] <- dctm[, srow] + ((-1)^(seq(ncep) - 1))
        dctm <- dctm/(2 * (srow + 1))
        return(dctm)
    }
    dctm1 <- function(spec, ncep) {
        srow <- nrow(spec)
        dctm <- cos((seq(ncep) - 1) * matrix(rep(0:(srow - 1), 
            each = ncep), nrow = ncep)/(srow - 1) * pi) * 2/(2 * 
            (srow - 1))
        dctm[, c(1, srow)] <- dctm[, c(1, srow)]/2
        return(dctm)
    }
    type <- match.arg(type)
    dctm <- switch(type, t1 = dctm1(spec, ncep), t2 = dctm2(spec, 
        ncep), t3 = dctm23(spec, ncep), t4 = dctm4(spec, ncep))
    cep <- dctm \%*\% log(spec)
    return(list(cep = cep, dctm = dctm))
  }
}
\keyword{ cepstra }
\keyword{ spectra }
\keyword{ conversion }
