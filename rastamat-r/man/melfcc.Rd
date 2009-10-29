\name{melfcc}
\alias{melfcc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
%[cepstra,aspectrum,pspectrum] = melfcc(samples, sr[, opts ...])
%  Calculate Mel-frequency cepstral coefficients by:
%   - take the absolute value of the STFT
%   - warp to a Mel frequency scale
%   - take the DCT of the log-Mel-spectrum
%   - return the first <ncep> components
%  This version allows a lot of options to be controlled, as optional 
%  'name', value pairs from the 3rd argument on: (defaults in parens)
%    'wintime' (0.025): window length in sec
%    'hoptime' (0.010): step between successive windows in sec
%    'numcep'     (13): number of cepstra to return
%    'lifterexp' (0.6): exponent for liftering; 0 = none; < 0 = HTK sin lifter
%    'sumpower'    (1): 1 = sum abs(fft)^2; 0 = sum abs(fft)
%    'preemph'  (0.97): apply pre-emphasis filter [1 -preemph] (0 = none)
%    'dither'      (0): 1 = add offset to spectrum as if dither noise
%    'minfreq'     (0): lowest band edge of mel filters (Hz)
%    'maxfreq'  (4000): highest band edge of mel filters (Hz)
%    'nbands'     (40): number of warped spectral bands to use
%    'bwidth'    (1.0): width of aud spec filters relative to default
%    'dcttype'     (2): type of DCT used - 1 or 2 (or 3 for HTK or 4 for feac)
%    'fbtype'  ('mel'): frequency warp: 'mel','bark','htkmel','fcmel'
%    'usecmp'      (0): apply equal-loudness weighting and cube-root compr.
%    'modelorder'  (0): if > 0, fit a PLP model of this order
% The following non-default values nearly duplicate Malcolm Slaney's mfcc
% (i.e. melfcc(d,16000,opts...) =~= log(10)*2*mfcc(d*(2^17),16000) )
%       'wintime': 0.016
%     'lifterexp': 0
%       'minfreq': 133.33
%       'maxfreq': 6855.6
%      'sumpower': 0
% The following non-default values nearly duplicate HTK's MFCC
% (i.e. melfcc(d,16000,opts...) =~= 2*htkmelfcc(:,[13,[1:12]])'
%  where HTK config has PREEMCOEF = 0.97, NUMCHANS = 20, CEPLIFTER = 22, 
%  NUMCEPS = 12, WINDOWSIZE = 250000.0, USEHAMMING = T, TARGETKIND = MFCC_0)
%     'lifterexp': -22
%        'nbands': 20
%       'maxfreq': 8000
%      'sumpower': 0
%        'fbtype': 'htkmel'
%       'dcttype': 3
% For more detail on reproducing other programs' outputs, see
% http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/mfccs.html
%
% 2005-04-19 dpwe@ee.columbia.edu after rastaplp.m.  
% Uses Mark Paskin's process_options.m from KPMtools
}
\usage{
melfcc(samples, sr = samples@samp.rate, wintime = 0.025, hoptime = 0.01, numcep = 13, lifterexp = 0.6, sumpower = TRUE, preemph = 0.97, dither = FALSE, minfreq = 0, maxfreq = sr/2, nbands = 40, bwidth = 1, dcttype = "t2", fbtype = "mel", usecmp = FALSE, modelorder = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{samples}{ ~~Describe \code{samples} here~~ }
  \item{sr}{ ~~Describe \code{sr} here~~ }
  \item{wintime}{ ~~Describe \code{wintime} here~~ }
  \item{hoptime}{ ~~Describe \code{hoptime} here~~ }
  \item{numcep}{ ~~Describe \code{numcep} here~~ }
  \item{lifterexp}{ ~~Describe \code{lifterexp} here~~ }
  \item{sumpower}{ ~~Describe \code{sumpower} here~~ }
  \item{preemph}{ ~~Describe \code{preemph} here~~ }
  \item{dither}{ ~~Describe \code{dither} here~~ }
  \item{minfreq}{ ~~Describe \code{minfreq} here~~ }
  \item{maxfreq}{ ~~Describe \code{maxfreq} here~~ }
  \item{nbands}{ ~~Describe \code{nbands} here~~ }
  \item{bwidth}{ ~~Describe \code{bwidth} here~~ }
  \item{dcttype}{ ~~Describe \code{dcttype} here~~ }
  \item{fbtype}{ ~~Describe \code{fbtype} here~~ }
  \item{usecmp}{ ~~Describe \code{usecmp} here~~ }
  \item{modelorder}{ ~~Describe \code{modelorder} here~~ }
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
