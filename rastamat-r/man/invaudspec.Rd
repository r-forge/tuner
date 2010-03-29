\name{invaudspec}
\alias{invaudspec}
\title{ Inverse frequency band conversion }
\description{ Invert the effects of \code{\link{audspec}}. }
\usage{
invaudspec(aspectrum, sr = 16000, nfft = 512, fbtype = "bark", minfreq = 0, maxfreq = -1, sumpower = TRUE, bdwidth = 1)
}
\arguments{
  \item{aspectrum}{Matrix with the auditory spectrum of each time frame in its
columns.}
  \item{sr}{ Samplerate of the original recording.}
  \item{nfft}{%%     ~~Describe \code{nfft} here~~
}
  \item{fbtype}{ Used auditory frequency scale.}
  \item{minfreq}{ Lowest frequency.}
  \item{maxfreq}{ Highest frequency. }
  \item{sumpower}{ If 'TRUE' base the frequency scale transformation on the
auditory spectrum, if 'FALSE' on its squareroot (absolute value of the spectrum) and
square it afterwards.
  \item{bdwidth}{ Modify the width of the frequency bands.}
}
\value{
  \item{spec }{Matrix with the powerspectrum
of each time frame in its columns.}
  \item{wts }{}
  \item{iwts }{}
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }
\seealso{\code{\link{audspec}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(aspectrum, sr = 16000, nfft = 512, fbtype = "bark", minfreq = 0, maxfreq = -1, sumpower = TRUE, bdwidth = 1){

  if (maxfreq == -1) {
    maxfreq <- sr/2
  }

  nfilts  <- nrow(aspectrum)
  nframes <- ncol(aspectrum)

  if (fbtype == "bark") {
    wts <- fft2barkmx(nfft, sr, nfilts, bwidth, minfreq, maxfreq)
  } else if (fbtype == "mel") {
    wts <- fft2melmx(nfft, sr, nfilts, bwidth, minfreq, maxfreq)
  } else if (fbtype == "htkmel") {
    wts <- fft2melmx(nfft, sr, nfilts, bwidth, minfreq, maxfreq, 1, 1)
  } else if (fbtype == "fcmel") {
    wts <- fft2melmx(nfft, sr, nfilts, bwidth, minfreq, maxfreq, 1)
  } else {
    stop("fbtype", fbtype, "not recognized")
  }
 
  # Cut off 2nd half
  wts <- wts[,1:((nfft/2)+1)]

  # Just transpose, fix up 
  ww   <- t(wts)\%*\%wts
  iwts <- t(wts)/kronecker(t(rep(1, nfilts)), t(max(mean(diag(ww))/100, sum(ww))))

  # Apply weights
  if (sumpower) {
    spec <- iwts \%*\% aspectrum
  } else {
    spec <- (iwts \%*\% sqrt(aspectrum))^2
  }

  return(list(spec=spec, wts=wts, iwts=iwts))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ frequency }
\keyword{ conversion }
\keyword{ mel }
\keyword{ bark }