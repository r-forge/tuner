\name{fft2melmx}
\alias{fft2melmx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
% wts = fft2melmx(nfft, sr, nfilts, width, minfrq, maxfrq, htkmel, constamp)
%      Generate a matrix of weights to combine FFT bins into Mel
%      bins.  nfft defines the source FFT size at sampling rate sr.
%      Optional nfilts specifies the number of output bands required 
%      (else one per bark), and width is the constant width of each 
%      band relative to standard Mel (default 1).
%      While wts has nfft columns, the second half are all zero. 
%      Hence, Mel spectrum is fft2melmx(nfft,sr)*abs(fft(xincols,nfft));
%      minfrq is the frequency (in Hz) of the lowest band edge;
%      default is 0, but 133.33 is a common standard (to skip LF).
%      maxfrq is frequency in Hz of upper edge; default sr/2.
%      You can exactly duplicate the mel matrix in Slaney's mfcc.m
%      as fft2melmx(512, 8000, 40, 1, 133.33, 6855.5, 0);
%      htkmel=1 means use HTK's version of the mel curve, not Slaney's.
%      constamp=1 means make integration windows peak at 1, not sum to 1.
% 2004-09-05  dpwe@ee.columbia.edu  based on fft2barkmx

}
\usage{
fft2melmx(nfft, sr = 8000, nfilts = 40, width = 1, minfrq = 0, maxfrq = sr/2, htkmel = FALSE, constamp = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nfft}{ ~~Describe \code{nfft} here~~ }
  \item{sr}{ ~~Describe \code{sr} here~~ }
  \item{nfilts}{ ~~Describe \code{nfilts} here~~ }
  \item{width}{ ~~Describe \code{width} here~~ }
  \item{minfrq}{ ~~Describe \code{minfrq} here~~ }
  \item{maxfrq}{ ~~Describe \code{maxfrq} here~~ }
  \item{htkmel}{ ~~Describe \code{htkmel} here~~ }
  \item{constamp}{ ~~Describe \code{constamp} here~~ }
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
function (nfft, sr = 8000, nfilts = 40, width = 1, minfrq = 0, 
    maxfrq = sr/2, htkmel = FALSE, constamp = FALSE) 
{
    fftfrqs <- (0:(nfft - 1))/nfft * sr
    minmel <- hz2mel(minfrq, htkmel)
    maxmel <- hz2mel(maxfrq, htkmel)
    binfrqs <- mel2hz(minmel + (0:(nfilts + 1))/(nfilts + 1) * 
        (maxmel - minmel), htkmel)
    binbin <- round(binfrqs/sr * (nfft - 1))
    wtscalc <- function(i, binfrqs = binfrqs) {
        fs <- binfrqs[i + c(0, 1, 2)]
        fs <- fs[2] + width * (fs - fs[2])
        loslope <- (fftfrqs - fs[1])/(fs[2] - fs[1])
        hislope <- (fs[3] - fftfrqs)/(fs[3] - fs[2])
        return(pmax(0, pmin(loslope, hislope)))
    }
    wts <- t(sapply(seq(nfilts), function(x) wtscalc(x, binfrqs)))
    if (!constamp) {
        wts <- diag(2/(binfrqs[2 + (1:nfilts)] - binfrqs[1:nfilts])) \%*\% 
            wts
    }
    return(list(wts = wts, binfrqs = binfrqs))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
