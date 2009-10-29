\name{fft2barkmx}
\alias{fft2barkmx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
% wts = fft2barkmx(nfft, sr, nfilts, width)
%      Generate a matrix of weights to combine FFT bins into Bark
%      bins.  nfft defines the source FFT size at sampling rate sr.
%      Optional nfilts specifies the number of output bands required 
%      (else one per bark), and width is the constant width of each 
%      band in Bark (default 1).
%      While wts has nfft columns, the second half are all zero. 
%      Hence, Bark spectrum is fft2barkmx(nfft,sr)*abs(fft(xincols,nfft));
% 2004-09-05  dpwe@ee.columbia.edu  based on rastamat/audspec.m
}
\usage{
fft2barkmx(nfft, sr = 8000, nfilts = NULL, width = 1, minfreq = 0, maxfreq = sr/2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nfft}{ ~~Describe \code{nfft} here~~ }
  \item{sr}{ ~~Describe \code{sr} here~~ }
  \item{nfilts}{ ~~Describe \code{nfilts} here~~ }
  \item{width}{ ~~Describe \code{width} here~~ }
  \item{minfreq}{ ~~Describe \code{minfreq} here~~ }
  \item{maxfreq}{ ~~Describe \code{maxfreq} here~~ }
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
function (nfft, sr = 8000, nfilts = NULL, width = 1, minfreq = 0, 
    maxfreq = sr/2) 
{
    min_bark <- hz2bark(minfreq)
    nyqbark <- hz2bark(maxfreq) - min_bark
    if (is.null(nfilts)) {
        nfilts <- ceiling(nyqbark) + 1
    }
    wts <- matrix(0, nrow = nfilts, ncol = nfft)
    step_barks <- nyqbark/(nfilts - 1)
    binbarks <- hz2bark((0:(nfft/2)) * sr/nfft)
    wtscalc <- function(i, min_bark = min_bark, step_barks = step_barks, 
        binbarks = binbarks) {
        f_bark_mid <- min_bark + (i - 1) * step_barks
        lof <- (binbarks - f_bark_mid)/width - 0.5
        hif <- (binbarks - f_bark_mid)/width + 0.5
        return(10^(pmin(0, pmin(hif, -2.5 * lof))))
    }
    wts[, 1:(nfft/2 + 1)] <- t(sapply(seq(nfilts), function(x) wtscalc(x, 
        min_bark, step_barks, binbarks)))
    return(list(wts = wts))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
