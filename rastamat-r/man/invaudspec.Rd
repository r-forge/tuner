\name{invaudspec}
\alias{invaudspec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
invaudspec(aspectrum, sr = 16000, nfft = 512, fbtype = "bark", minfreq = 0, maxfreq = -1, sumpower = TRUE, bdwidth = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aspectrum}{
%%     ~~Describe \code{aspectrum} here~~
}
  \item{sr}{
%%     ~~Describe \code{sr} here~~
}
  \item{nfft}{
%%     ~~Describe \code{nfft} here~~
}
  \item{fbtype}{
%%     ~~Describe \code{fbtype} here~~
}
  \item{minfreq}{
%%     ~~Describe \code{minfreq} here~~
}
  \item{maxfreq}{
%%     ~~Describe \code{maxfreq} here~~
}
  \item{sumpower}{
%%     ~~Describe \code{sumpower} here~~
}
  \item{bdwidth}{
%%     ~~Describe \code{bdwidth} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
