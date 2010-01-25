\name{lpc2spec}
\alias{lpc2spec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
% [features,F,M] = lpc2spec(lpcas,nout)
%    Convert LPC coeffs back into spectra
%    nout is number of freq channels, default 17 (i.e. for 8 kHz)
% 2003-04-11 dpwe@ee.columbia.edu  part of rastamat

}
\usage{
lpc2spec(lpcas, nout = 17)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lpcas}{
%%     ~~Describe \code{lpcas} here~~
}
  \item{nout}{
%%     ~~Describe \code{nout} here~~
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
function(lpcas, nout = 17) {

  rows <- nrow(lpcas)
  cols <- ncol(lpcas)
  order <- rows - 1

  gg <- lpcas[1,]
  aa <- t(t(lpcas) / gg)

  # Calculate the actual z-plane polyvals: nout points around unit circle
  zz <- exp( (-1i * (0:(nout-1)) * pi / (nout-1)) \%*\% t(0:order))

  # Actual polyvals, in power (mag^2)
  features <- t(t((1/abs(zz \%*\% aa))^2) / gg)

  F <- matrix(0, cols, floor(rows/2)) 
  M <- F

  for (c in 1:cols) {
    aaa <- aa[,c]
    rr <- polyroot(rev(t(aaa)))
    ff <- Arg(t(rr))

    zz <- exp(1i * t(ff) \%*\% (0:(length(aaa)-1)))
    mags <- sqrt(t( ((1/abs(zz\%*\%aaa))^2) / gg[c] ))
  
    ix <- order(ff)
    keep <- ff[ix] > 0
    ix <- ix[keep]
    F[c,1:length(ix)] <- ff[ix]
    M[c,1:length(ix)] <- mags[ix]
  }

  return(list(features=features, F=F, M=M))

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
