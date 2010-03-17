\name{postaud}
\alias{postaud}
\title{ Equal loudness compression }
\description{
Do loudness equalization and cube root compression
}
\usage{
postaud(x, fmax, fbtype = c("mel", "bark", "htkmel", "fcmel"), broaden = FALSE)
}
\arguments{
  \item{x}{ Matrix of spectra (output of \code{\link{audspec}}). }
  \item{fmax}{ Maximum frequency im Hertz. }
  \item{fbtype}{ Auditory frequency scale. }
  \item{broaden}{ Use two additional frequency bands for calculation. }
}
\value{
  \item{x }{Matrix of the per sample/frame (columns) spectra after applying the frequency
dependant loudness equalization and compression. }
  \item{eql }{Vector of the equal loudness curve. }
}
\references{Daniel P. W. Ellis
\url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/}, Hynek
Hermansky }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{ \code{\link{audspec}}, \code{\link{dolpc}}, \code{\link{rastaplp}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, fmax, fbtype = c("mel", "bark", "htkmel", "fcmel"), 
    broaden = FALSE) 
{
    nbands <- nrow(x)
    nframes <- ncol(x)
    nfpts <- nbands + 2 * broaden
    fbtype <- match.arg(fbtype)
    bandcfhz <- switch(fbtype, bark = bark2hz(seq(0, hz2bark(fmax), 
        length.out = nfpts)), mel = mel2hz(seq(0, hz2mel(fmax), 
        length.out = nfpts)), htkmel = mel2hz(seq(0, hz2mel(fmax, 
        TRUE), length.out = nfpts), TRUE), fcmel = mel2hz(seq(0, 
        hz2mel(fmax, TRUE), length.out = nfpts), TRUE), )
    bandcfhz <- bandcfhz[(1 + broaden):(nfpts - broaden)]
    fsq <- bandcfhz^2
    ftmp <- fsq + 160000
    eql <- ((fsq/ftmp)^2) * ((fsq + 1440000)/(fsq + 9610000))
    z <- matrix(rep(eql, nframes), ncol = nframes) * x
    z <- z^(0.33)
    if (broaden) {
        y <- z[c(1, 1:nbands, nbands), ]
    }
    else {
        y <- z[c(2, 2:(nbands - 1), nbands - 1), ]
    }
    return(list(y = y, eql = eql))
  }
}
\keyword{ plp }
\keyword{ loudness }
\keyword{ compression }
