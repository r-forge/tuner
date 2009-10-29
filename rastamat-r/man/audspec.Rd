\name{audspec}
\alias{audspec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
%[aspectrum,wts] = audspec(pspectrum, sr, nfilts, fbtype, minfreq, maxfreq, sumpower, bwidth)
%
% perform critical band analysis (see PLP)
% takes power spectrogram as input
}
\usage{
audspec(pspectrum, sr = 16000, nfilts = ceiling(hz2bark(sr/2)) + 1, fbtype = c("bark", "mel", "htkmel", "fcmel"), minfreq = 0, maxfreq = sr/2, sumpower = TRUE, bwidth = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pspectrum}{ Output of \code{\link{powspec}}, matrix with the powerspectrum
of each time frame in its columns. }
  \item{sr}{ Samplerate of the original recording.  }
  \item{nfilts}{ Number of filters/frequency bins in the auditory frequency scale. }
  \item{fbtype}{ Used auditory frequency scale. }
  \item{minfreq}{ Lowest frequency. }
  \item{maxfreq}{ Highest frequency. }
  \item{sumpower}{ If 'TRUE' base the frequency scale transformation on the
powerspectrum, if 'FALSE' on its squareroot (absolute value if the spectrum) and
square it afterwards. }
  \item{bwidth}{ ~~Describe \code{bwidth} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
