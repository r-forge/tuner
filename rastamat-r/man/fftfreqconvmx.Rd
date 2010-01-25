\name{fftfreqconvmx}
\alias{fft2barkmx}
\alias{fft2melmx}
\title{ Convert FFT frequency bins to Bark/Mel bins }
\description{
 Generate a matrix of weights to combine FFT bins into Bark bins.
}
\usage{
fft2barkmx(nfft, sr = 8000, nfilts = NULL, width = 1, minfreq = 0, maxfreq = sr/2)
fft2melmx(nfft, sr = 8000, nfilts = 40, width = 1, minfrq = 0, maxfrq = sr/2, htkmel = FALSE, constamp = FALSE)
}
\arguments{
  \item{nfft}{ Source FFT size. }
  \item{sr}{ Sampling rate of the signal. }
  \item{nfilts}{ Number of desired output frequency bands. If 'NULL' then one
per Bark/Mel between 'minfreq' and 'maxfreq'. }
  \item{width}{ Width of each output frequency band in Bark/Mel. }
  \item{minfreq}{ Minimum frequency. }
  \item{maxfreq}{ Maximum frequency. }
  \item{htkmel}{ Use HTK- or Slaney's curve of the Melscale. }
  \item{constamp}{ Make integration windows peak at 1 ('TRUE') or sum to 1 ('FALSE') }
}
\value{
  \item{wts }{The weight matrix with 'nfft' columns and 'nfilts' rows.}
  \item{binfrqs }{Edge frequencies of the bins.}
}
\references{Daniel P. W. Ellis:
\url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/}, Malcolm
Slaney: Auditory Toolbox }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\note{
      While wts has nfft columns, the second half are all zero. 
      Hence, Bark spectrum is fft2barkmx(nfft,sr)*abs(fft(xincols,nfft)).

      You can exactly duplicate the mel matrix in Slaney's mfcc.m
      as fft2melmx(512, 8000, 40, 1, 133.33, 6855.5, FALSE, FALSE).
}
\seealso{ \code{\link{hz2bark}}, \code{\link{hz2mel}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{ fft }
\keyword{ frequency }
\keyword{ bark }
\keyword{ mel }
\keyword{ conversion }
