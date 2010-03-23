\name{powspec}
\alias{powspec}
\title{ Powerspectrum }
\description{
Compute the powerspectrum of the input signal.
Basically output a power spectrogram using a Hamming window.
}
\usage{
powspec(x, sr = 8000, wintime = 0.025, steptime = 0.01, dither = FALSE)
}
\arguments{
  \item{x}{ Vector of samples. }
  \item{sr}{ Sampling rate of the signal. }
  \item{wintime}{ Window length in sec. }
  \item{steptime}{ Step between successive windows in sec. }
  \item{dither}{ Add offset to spectrum as if dither noise. }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
Matrix, where each column represents a power spectrum for a given frame
and each row represents a frequency.
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{ \code{\link{specgram}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, sr = 8000, wintime = 0.025, steptime = 0.01, dither = FALSE) 
{
    winpts <- round(wintime * sr)
    steppts <- round(steptime * sr)
    nfft <- 2^(ceiling(log(winpts)/log(2)))
    window <- hamming.window(winpts)
    noverlap <- winpts - steppts
    y <- abs(specgram(x, nfft, sr, window, noverlap)$S)^2
    if (dither) {
        y <- y + winpts
    }
    return(y)
  }
}
\keyword{ powerspectrum }
