\name{rastaplp}
\alias{rastaplp}
\title{RASTA}
\description{ log rasta with fixed parameters }
\usage{
rastaplp(samples, sr = samples@samp.rate, dorasta = TRUE,
            modelorder= 8, wintime=0.025, hoptime=0.010,
            lifterexp=0.6, HTKlifter=FALSE,
            sumpower=TRUE, dither=TRUE,
            minfreq=0, maxfreq=sr/2, nbands=ceiling(hz2bark(sr/2))+1, bwidth=1.0,
            fbtype="bark", spec_out=FALSE, frames_in_rows=TRUE) {
}
\arguments{
  \item{samples}{ Object of class 'Wave'.}
  \item{sr}{ Sampling rate of the signal}
  \item{dorasta}{ Use rasta filtering or not. }
  \item{modelorder}{ If > 0, fit a linear prediction (autoregressive-) model of this order and
calculation of cepstra out of 'lpcas' }
  \item{wintime}{ Window length in sec. }
  \item{hoptime}{ Step between successive windows in sec. }
  \item{lifterexp}{ Exponent for liftering; 0 = none. }
  \item{HTKlifter}{ Use HTK sin lifter. }
  \item{sumpower}{ 'TRUE': sum abs(fft)^2; 'FALSE': sum abs(fft) }
  \item{dither}{ Add offset to spectrum as if dither noise. }
  \item{minfreq}{ Lowest band edge of mel filters (Hz). }
  \item{maxfreq}{ Highest band edge of mel filters (Hz). }
  \item{nbands}{ Number of warped spectral bands to use. }
  \item{bwidth}{ Width of spectral bands in Bark/Mel. }
  \item{dcttype}{ Type of DCT used - 1 or 2 (or 3 for HTK or 4 for feacalc). }
  \item{fbtype}{ Auditory frequency scale to use: 'mel','bark','htkmel','fcmel' }
  \item{spec_out}{ Should matrices of the power- and the auditory-spectrum be
    returned}
  \item{frames_in_rows}{ Return time frames in rows instead of columns (original Matlab code)}
}
\value{
  \item{cepstra }{Matrix of resulting cepstra.}
  \item{spectra }{Matrix of resulting spectra.}
  \item{pspectrum }{Matrix of resulting power spectrum.}
  \item{lpcas }{Matrix of the normalized AR coefficients.}
  \item{Fout }{ Argument of frequencies in complex plane }
  \item{Mout }{ Magnitude of frequencies in complex plane }
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }
\seealso{ \code{\link{powspec}}, \code{\link{audpec}}, \code{\link{aspectrum}}, \code{\link{rastafilt}}, \code{\link{dolpc}}, \code{\link{lpc2spec}} }
\examples{
  testsound <- normalize(sine(400) + sine(1000) + square(250), "16")
  r1 <- rastaplp(testsound)

  # normal PLPs (without RASTA filtering)
  r2 <- rastaplp(testsound, dorasta=FALSE)
}
\concept{ spectrum }
\concept{ rasta }
\concept{ cepstrum }
