\name{invpowspec}
\alias{invpowspec}
\title{ Inverse Powerspectrum }
}
\description{ Restore the audio signal out of a powerspectrum computed of it. }
\usage{
invpowspec(y, sr = 8000, wintime = 0.025, steptime = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ Matrix, where each column represents a power spectrum for a given frame
and each row represents a frequency.}
  \item{sr}{ Sampling rate of the signal. }
  \item{wintime}{ Window length in sec. }
  \item{steptime}{ Step between successive windows in sec. }
}
\value{Vector of samples representing the restored signal.}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }
\seealso{ \code{\link{powspec}}, \code{\link{specgram}}, \code{\link{ispecgram}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(y, sr = 8000, wintime = 0.025, steptime = 0.010){

  nrow <- nrow(y)
  ncol <- ncol(y)

  winpts <- round(wintime*sr)
  steppts <- round(steptime*sr)

  NFFT <- 2^(ceiling(log(winpts)/log(2)))

  if (NFFT != 2*(nrow-1)) {
    warning("Inferred FFT size doesn't match specgram")
  }

  NOVERLAP <- winpts - steppts
  SAMPRATE <- sr

  # Values coming out of rasta treat samples as integers, 
  # not range -1..1, hence scale up here to match (approx)
  # y = abs(specgram(x*32768,NFFT,SAMPRATE,WINDOW,NOVERLAP))^2

  xlen <- winpts + steppts*(ncol - 1)

  r <- t(rnorm(xlen))

  R <- specgram(r/32768/12, NFFT, SAMPRATE, winpts, NOVERLAP)$S
  R = R * sqrt(y)
  x = ispecgram(R, NFFT, SAMPRATE, winpts, NOVERLAP)

  return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ powerspectrum }
\keyword{ inverse }
\keyword{ frequency }
