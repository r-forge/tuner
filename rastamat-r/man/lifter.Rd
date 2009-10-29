\name{lifter}
\alias{lifter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~

% y = lifter(x, lift, invs)
%   Apply lifter to matrix of cepstra (one per column)
%   lift = exponent of x i^n liftering
%   or, as a negative integer, the length of HTK-style sin-curve liftering.
%   If inverse == 1 (default 0), undo the liftering.
% 2005-05-19 dpwe@ee.columbia.edu
}
\usage{
lifter(x, lift = 0.6, inv = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{lift}{ ~~Describe \code{lift} here~~ }
  \item{inv}{ ~~Describe \code{inv} here~~ }
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
function (x, lift = 0.6, inv = FALSE) 
{
    ncep <- nrow(x)
    if (lift == 0) {
        y <- x
    }
    else if (lift > 0) {
        liftwts <- c(1, (1:(ncep - 1))^lift)
    }
    else if (lift < 0) {
        L <- -lift
        if (L - as.integer(L) == 0) {
            liftwts <- c(1, (1 + L/2 * sin((1:(ncep - 1)) * pi/L)))
        }
        else {
            stop("HTK liftering value must be integer!")
        }
    }
    if (inv) {
        liftwts <- 1/liftwts
    }
    y <- diag(liftwts) \%*\% x
    return(y)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
