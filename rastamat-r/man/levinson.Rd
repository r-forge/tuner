\name{levinson}
\alias{levinson}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
## usage:  [a, v, ref] = levinson (acf [, p])
##
## Use the Durbin-Levinson algorithm to solve:
##    toeplitz(acf(1:p)) * x = -acf(2:p+1).
## The solution [1, x'] is the denominator of an all pole filter
## approximation to the signal x which generated the autocorrelation
## function acf.  
##
## acf is the autocorrelation function for lags 0 to p.
## p defaults to length(acf)-1.
## Returns 
##   a=[1, x'] the denominator filter coefficients. 
##   v= variance of the white noise = square of the numerator constant
##   ref = reflection coefficients = coefficients of the lattice
##         implementation of the filter
## Use freqz(sqrt(v),a) to plot the power spectrum.
   
## Author:  Paul Kienzle <pkienzle@users.sf.net>

## TODO: Matlab doesn't return reflection coefficients and 
## TODO:    errors in addition to the polynomial a.
## TODO: What is the difference between aryule, levinson, 
## TODO:    ac2poly, ac2ar, lpc, etc.?
  
## Changes (Peter Lanspeary, 6 Nov 2006):
##  Add input sanitising;
##  avoid using ' (conjugate transpose) to force a column vector;
##  take direct solution from Kay & Marple Eqn (2.39), code is now same
##        as the theory;
##  take Durbin-Levinson recursion from Kay & Marple Eqns (2.42-2.46),
##        now works for complex data, code is now same as the theory;
##  force real variance (get rid of imaginary part due to rounding error);
##
## REFERENCE
## [1] Steven M. Kay and Stanley Lawrence Marple Jr.:
##   "Spectrum analysis -- a modern perspective",
##   Proceedings of the IEEE, Vol 69, pp 1380-1419, Nov., 1981

}
\usage{
levinson(x, p = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{p}{ ~~Describe \code{p} here~~ }
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
function (x, p = NULL) 
{
    fit <- function(acf, p) {
        ref <- numeric(p)
        g <- -acf[2]/acf[1]
        a <- g
        v <- Re((1 - g * Conj(g)) * acf[1])
        ref[1] <- g
        for (t in 2:p) {
            g <- -(acf[t + 1] + a \%*\% acf[seq(t, 2, -1)])/v
            a <- c((a + g * Conj(a[seq(t - 1, 1, -1)])), g)
            v <- v * (1 - Re(g * Conj(g)))
            ref[t] <- g
        }
        a <- c(1, a)
        return(list(a = a, v = v, ref = ref))
    }
    if (!is.null(p) && (p - as.integer(p) != 0)) 
        stop("p must be integer.")
    if (is.vector(x)) {
        lx <- length(x)
        if (is.null(p) || p >= lx) 
            p <- lx - 1
        r <- fit(x, p)
    }
    else {
        if (is.matrix(x)) {
            lx <- dim(x)
            if (is.null(p) || p >= lx[1]) 
                p <- lx[1] - 1
            zr <- apply(x, 2, function(y) fit(y, p))
            zr <- matrix(unlist(zr), nrow = lx[2], byrow = TRUE)
            a <- zr[, 1:(p + 1)]
            v <- zr[, p + 2]
            ref <- t(zr[, -(1:(p + 2))])
            r <- list(a = a, v = v, ref = ref)
        }
        else {
            stop("x must be a vector or matrix.")
        }
    }
    return(r)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
