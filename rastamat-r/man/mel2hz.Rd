\name{mel2hz}
\alias{mel2hz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
%   f = mel2hz(z, htk)
%   Convert 'mel scale' frequencies into Hz
%   Optional htk = 1 means use the HTK formula
%   else use the formula from Slaney's mfcc.m
}
\usage{
mel2hz(z, htk = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ ~~Describe \code{z} here~~ }
  \item{htk}{ ~~Describe \code{htk} here~~ }
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
function (z, htk = FALSE) 
{
    if (htk) {
        f <- 700 * (10^(z/2595) - 1)
    }
    else {
        f_0 <- 0
        f_sp <- 200/3
        brkfrq <- 1000
        brkpt <- (brkfrq - f_0)/f_sp
        logstep <- exp(log(6.4)/27)
        linpts <- (z < brkpt)
        f <- 0 * z
        f[linpts] = f_0 + f_sp * z[linpts]
        f[!linpts] <- brkfrq * exp(log(logstep) * (z[!linpts] - 
            brkpt))
    }
    return(f)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
