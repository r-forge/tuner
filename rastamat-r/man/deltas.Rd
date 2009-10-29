\name{deltas}
\alias{deltas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
% D = deltas(X,W)  Calculate the deltas (derivatives) of a sequence
%    Use a W-point window (W odd, default 9) to calculate deltas using a
%    simple linear slope.  This mirrors the delta calculation performed 
%    in feacalc etc.  Each row of X is filtered separately.
% 2003-06-30 dpwe@ee.columbia.edu

}
\usage{
deltas(x, w = 9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{w}{ ~~Describe \code{w} here~~ }
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
function (x, w = 9) 
{
    nr <- nrow(x)
    nc <- ncol(x)
    hlen <- floor(w/2)
    w <- 2 * hlen + 1
    win <- seq(hlen, -hlen, -1)
    xx <- matrix(c(rep(x[, 1], hlen), x, rep(x[, 1], hlen)), 
        nrow = nr)
    d <- t(apply(xx, 1, function(x) {
        convolve(x, win, conj = FALSE, type = "open")[-(1:(length(win) - 
            1))]
    }))
    d <- d[, 2 * hlen + 1:nc]
    return(d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
