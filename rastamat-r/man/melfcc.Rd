\name{melfcc}
\alias{melfcc}
\title{ MFCC Calculation }
\description{
  Calculate Mel-frequency cepstral coefficients.
}
\usage{
melfcc(samples, sr = samples@samp.rate, wintime = 0.025, hoptime = 0.01, numcep = 13, lifterexp = 0.6, sumpower = TRUE, preemph = 0.97, dither = FALSE, minfreq = 0, maxfreq = sr/2, nbands = 40, bwidth = 1, dcttype = "t2", fbtype = "mel", usecmp = FALSE, modelorder = 0)
}
\arguments{
  \item{samples}{ Object of class 'Wave'. }
  \item{sr}{ Sampling rate of the signal. }
  \item{wintime}{ Window length in sec. }
  \item{hoptime}{ Step between successive windows in sec. }
  \item{numcep}{ Number of cepstra to return. }
  \item{lifterexp}{ Exponent for liftering; 0 = none; < 0 = HTK sin lifter. }
  \item{sumpower}{ 'TRUE': sum abs(fft)^2; 'FALSE': sum abs(fft) }
  \item{preemph}{ Apply pre-emphasis filter [1 -preemph] (0 = none). }
  \item{dither}{ Add offset to spectrum as if dither noise. }
  \item{minfreq}{ Lowest band edge of mel filters (Hz). }
  \item{maxfreq}{ Highest band edge of mel filters (Hz). }
  \item{nbands}{ Number of warped spectral bands to use. }
  \item{bwidth}{ Width of spectral bands in Bark/Mel. }
  \item{dcttype}{ Type of DCT used - 1 or 2 (or 3 for HTK or 4 for feac). }
  \item{fbtype}{ Auditory frequency scale to use: 'mel','bark','htkmel','fcmel' }
  \item{usecmp}{ Apply equal-loudness weighting and cube-root compression (PLP
instead of LPC). }
  \item{modelorder}{ If > 0, fit a linear prediction model of this order and
calculation of cepstra out of 'lpcas' }
}
\details{
Calculation of the MFCCs imlcudes following steps:
\enumerate{
  \item{Preemphasis filtering}
  \item{Take the absolute value of the STFT (usage of Hamming window)}
  \item{Warp to Mel frequency scale}
  \item{Take the DCT of the log-Mel-spectrum}
  \item{Return the first 'ncep' components}
  }
}
\value{
  \item{cepstra }{Cepstral coefficients of the input signal (one time frame per
column)}
  \item{aspectrum }{Auditory spectrum (spectrum after transformation to Mel
scale) of the signal}
  \item{pspectrum }{Power spectrum of the input signal.}
  \item{lpcas }{If 'modelorder > 0' the linear prediction coefficients (LPC/PLP).}
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\note{
The following non-default values nearly duplicate Malcolm Slaney's mfcc
(i.e. melfcc(d,16000,opts...) =~= log(10)*2*mfcc(d*(2^17),16000) )
       'wintime': 0.016
     'lifterexp': 0
       'minfreq': 133.33
       'maxfreq': 6855.6
      'sumpower': 0
The following non-default values nearly duplicate HTK's MFCC
(i.e. melfcc(d,16000,opts...) =~= 2*htkmelfcc(:,[13,[1:12]])'
 where HTK config has PREEMCOEF = 0.97, NUMCHANS = 20, CEPLIFTER = 22, 
 NUMCEPS = 12, WINDOWSIZE = 250000.0, USEHAMMING = T, TARGETKIND = MFCC_0)
     'lifterexp': -22
        'nbands': 20
       'maxfreq': 8000
      'sumpower': 0
        'fbtype': 'htkmel'
       'dcttype': 3
For more detail on reproducing other programs' outputs, see
\url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/mfccs.html}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (samples, sr = samples@samp.rate, wintime = 0.025, hoptime = 0.01, 
    numcep = 13, lifterexp = 0.6, sumpower = TRUE, preemph = 0.97, 
    dither = FALSE, minfreq = 0, maxfreq = sr/2, nbands = 40, 
    bwidth = 1, dcttype = "t2", fbtype = "mel", usecmp = FALSE, 
    modelorder = 0) 
{
    if (preemph != 0) {
        ssamples <- filter(samples@left, filter = c(1, -preemph), 
            method = "convolution", sides = 1, circular = F)
        ssamples[1] <- samples@left[1]
    }
    else {
        ssamples <- samples@left
    }
    pspectrum <- powspec(ssamples, sr, wintime, hoptime, dither)
    aspectrum <- audspec(pspectrum, sr, nbands, fbtype, minfreq, 
        maxfreq, sumpower, bwidth)$aspectrum
    if (usecmp) {
        aspectrum <- postaud(aspectrum, maxfreq, fbtype)$y
    }
    lpcas <- NULL
    if (modelorder > 0) {
        if (dcttype != "t1") {
            warning("PLP cepstra are implicitly dcttype 1")
        }
        lpcas <- dolpc(aspectrum, modelorder)
        cepstra <- lpc2cep(lpcas, numcep)
    }
    else {
        cepstra <- spec2cep(aspectrum, numcep, dcttype)$cep
    }
    cepstra <- lifter(cepstra, lifterexp)
    return(list(cepstra = cepstra, aspectrum = aspectrum, pspectrum = pspectrum, 
        lpcas = lpcas))
  }
}
\keyword{ mfcc }
\keyword{ lpc }
\keyword{ plp }
