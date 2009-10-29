\name{powspec}
\alias{powspec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~

%y = powspec(x, sr, wintime, steptime, sumlin, dither)
%
% compute the powerspectrum of the input signal.
% basically outputs a power spectrogram
%
% each column represents a power spectrum for a given frame
% each row represents a frequency
%
% default values:
% sr = 8000Hz
% wintime = 25ms (200 samps)
% steptime = 10ms (80 samps)
% which means use 256 point fft
% hamming window

}
\usage{
powspec(x, sr = 8000, wintime = 0.025, steptime = 0.01, dither = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{sr}{ ~~Describe \code{sr} here~~ }
  \item{wintime}{ ~~Describe \code{wintime} here~~ }
  \item{steptime}{ ~~Describe \code{steptime} here~~ }
  \item{dither}{ ~~Describe \code{dither} here~~ }
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
function (x, sr = 8000, wintime = 0.025, steptime = 0.01, dither = TRUE) 
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
