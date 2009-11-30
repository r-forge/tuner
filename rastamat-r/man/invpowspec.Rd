\name{invpowspec}
\alias{invpowspec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
invpowspec(y, sr = 8000, wintime = 0.025, steptime = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{sr}{
%%     ~~Describe \code{sr} here~~
}
  \item{wintime}{
%%     ~~Describe \code{wintime} here~~
}
  \item{steptime}{
%%     ~~Describe \code{steptime} here~~
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
function(y, sr = 8000, wintime = 0.025, steptime = 0.010){

  nrow <- nrow(y)
  ncol <- ncol(y)

  winpts <- round(wintime*sr)
  steppts <- round(steptime*sr)

  NFFT <- 2^(ceiling(log(winpts)/log(2)))

  if (NFFT != 2*(nrow-1)) {
    warning("Inferred FFT size doesn't match specgram")
  }

  NOVERLAP <- winpts - steppts
  SAMPRATE <- sr

  # Values coming out of rasta treat samples as integers, 
  # not range -1..1, hence scale up here to match (approx)
  # y = abs(specgram(x*32768,NFFT,SAMPRATE,WINDOW,NOVERLAP))^2

  xlen <- winpts + steppts*(ncol - 1)

  r <- t(rnorm(xlen))

  R <- specgram(r/32768/12, NFFT, SAMPRATE, winpts, NOVERLAP)$S
  R = R * sqrt(y)
  x = ispecgram(R, NFFT, SAMPRATE, winpts, NOVERLAP)

  return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
