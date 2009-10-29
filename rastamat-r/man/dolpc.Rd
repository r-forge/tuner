\name{dolpc}
\alias{dolpc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
%y = dolpc(x,modelorder)
%
% compute autoregressive model from spectral magnitude samples
%
% rows(x) = critical band
% col(x) = frame
%
% row(y) = lpc a_i coeffs, scaled by gain
% col(y) = frame
%
% modelorder is order of model, defaults to 8
% 2003-04-12 dpwe@ee.columbia.edu after shire@icsi.berkeley.edu

}
\usage{
dolpc(x, modelorder = 8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
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
function (x, modelorder = 8) 
{
    nbands <- nrow(x)
    px <- planFFT(2 * nbands - 2)
    r <- apply(rbind(x, x[seq(nbands - 1, 2, -1), ]), 2, function(y) Re(IFFT(y, 
        plan = px)))
    r <- r[1:nbands, ]
    levcoef <- levinson(r, modelorder)
    y <- t(levcoef$a)/matrix(rep(levcoef$v, modelorder + 1), 
        nrow = modelorder + 1, byrow = TRUE)
    return(y)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
