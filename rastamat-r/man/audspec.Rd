\name{audspec}
\alias{audspec}
\title{Frequency band conversion }
\description{
Perform critical band analysis (see PLP), which means the reduction of the
fourier frequencies of a signal's powerspectrum to a reduced number of frequency
bands in an auditory frequency scale.
}
\usage{
audspec(pspectrum, sr = 16000, nfilts = ceiling(hz2bark(sr/2)) + 1, fbtype = c("bark", "mel", "htkmel", "fcmel"), minfreq = 0, maxfreq = sr/2, sumpower = TRUE, bwidth = 1)
}
\arguments{
  \item{pspectrum}{ Output of \code{\link{powspec}}, matrix with the powerspectrum
of each time frame in its columns. }
  \item{sr}{ Samplerate of the original recording.  }
  \item{nfilts}{ Number of filters/frequency bins in the auditory frequency scale. }
  \item{fbtype}{ Used auditory frequency scale. }
  \item{minfreq}{ Lowest frequency. }
  \item{maxfreq}{ Highest frequency. }
  \item{sumpower}{ If 'TRUE' base the frequency scale transformation on the
powerspectrum, if 'FALSE' on its squareroot (absolute value of the spectrum) and
square it afterwards. }
  \item{bwidth}{ Modify the width of the frequency bands. }
}
\value{
  \item{aspectrum }{Matrix with the auditory spectrum of each time frame in its
columns.}
  \item{wts }{Weight matrix for the frequency band conversion.}
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{\code{\link{fft2melmx}}, \code{\link{fft2barkmx}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (pspectrum, sr = 16000, nfilts = ceiling(hz2bark(sr/2)) + 
    1, fbtype = c("bark", "mel", "htkmel", "fcmel"), minfreq = 0, 
    maxfreq = sr/2, sumpower = TRUE, bwidth = 1) 
{
    nfreqs <- nrow(pspectrum)
    nfft <- (nfreqs - 1) * 2
    fbtype <- match.arg(fbtype)
    wts <- switch(fbtype, bark = fft2barkmx(nfft, sr, nfilts, 
        bwidth, minfreq, maxfreq)$wts, mel = fft2melmx(nfft, 
        sr, nfilts, bwidth, minfreq, maxfreq)$wts, htkmel = fft2melmx(nfft, 
        sr, nfilts, bwidth, minfreq, maxfreq, TRUE, TRUE)$wts, 
        fcmel = fft2melmx(nfft, sr, nfilts, bwidth, minfreq, 
            maxfreq, TRUE, FALSE)$wts)
    wts <- wts[, 1:nfreqs]
    if (sumpower) {
        aspectrum <- wts \%*\% pspectrum
    }
    else {
        aspectrum <- (wts \%*\% sqrt(psectrum))^2
    }
    return(list(aspectrum = aspectrum, wts = wts))
  }
}
\keyword{ frequency }
\keyword{ conversion }
\keyword{ bark }
\keyword{ mel }
