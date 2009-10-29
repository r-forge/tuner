\name{postaud}
\alias{postaud}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
%y = postaud(x, fmax, fbtype)
%
% do loudness equalization and cube root compression
% x = critical band filters
% rows = critical bands
% cols = frames


}
\usage{
postaud(x, fmax, fbtype = c("mel", "bark", "htkmel", "fcmel"), broaden = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{fmax}{ ~~Describe \code{fmax} here~~ }
  \item{fbtype}{ ~~Describe \code{fbtype} here~~ }
  \item{broaden}{ ~~Describe \code{broaden} here~~ }
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
function (x, fmax, fbtype = c("mel", "bark", "htkmel", "fcmel"), 
    broaden = FALSE) 
{
    nbands <- nrow(x)
    nframes <- ncol(x)
    nfpts <- nbands + 2 * broaden
    fbtype <- match.arg(fbtype)
    bandcfhz <- switch(fbtype, bark = bark2hz(seq(0, hz2bark(fmax), 
        length.out = nfpts)), mel = mel2hz(seq(0, hz2mel(fmax), 
        length.out = nfpts)), htkmel = mel2hz(seq(0, hz2mel(fmax, 
        TRUE), length.out = nfpts), TRUE), fcmel = mel2hz(seq(0, 
        hz2mel(fmax, TRUE), length.out = nfpts), TRUE), )
    bandcfhz <- bandcfhz[(1 + broaden):(nfpts - broaden)]
    fsq <- bandcfhz^2
    ftmp <- fsq + 160000
    eql <- ((fsq/ftmp)^2) * ((fsq + 1440000)/(fsq + 9610000))
    z <- matrix(rep(eql, nframes), ncol = nframes) * x
    z <- z^(0.33)
    if (broaden) {
        y <- z[c(1, 1:nbands, nbands), ]
    }
    else {
        y <- z[c(2, 2:(nbands - 1), nbands - 1), ]
    }
    return(list(y = y, eql = eql))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
