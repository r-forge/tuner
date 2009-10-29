\name{lpc2cep}
\alias{lpc2cep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~

% features = lpc2cep(lpcas,nout)
%    Convert the LPC 'a' coefficients in each column of lpcas
%    into frames of cepstra.
%    nout is number of cepstra to produce, defaults to size(lpcas,1)
% 2003-04-11 dpwe@ee.columbia.edu
}
\usage{
lpc2cep(a, nout = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{ ~~Describe \code{a} here~~ }
  \item{nout}{ ~~Describe \code{nout} here~~ }
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
function (a, nout = NULL) 
{
    arow <- nrow(a)
    acol <- ncol(a)
    order <- arow - 1
    if (is.null(nout)) {
        nout <- order + 1
    }
    mc <- matrix(0, nrow = nout, ncol = acol)
    mc[1, ] = -log(a[1, ])
    a <- a/matrix(rep(a[1, ], arow), nrow = arow, byrow = TRUE)
    for (n in 2:nout) {
        s <- 0
        for (m in 2:n) {
            s <- s + (n - m) * a[m, ] * mc[n - m + 1, ]
        }
        mc[n, ] <- -(a[n, ] + s/(n - 1))
    }
    features <- mc
    return(features)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
