\name{levinson}
\alias{levinson}
\title{ Durbin-Levinson Recursion }
\description{
Perform Durbin-Levinson recursion on vector or matrix.
}
\usage{
levinson(x, p = NULL)
}
\arguments{
  \item{x}{ Input signal. }
  \item{p}{ Lag (defaults to \code{length(x)} or \code{nrow(x)}). }
}
\details{
 Use the Durbin-Levinson algorithm to solve:

    \code{toeplitz(acf(1:p)) * y = -acf(2:p+1).}

 The solution [1, y'] is the denominator of an all pole filter
 approximation to the signal x which generated the autocorrelation
 function acf.  

 acf is the autocorrelation function for lags 0 to p.
}
\value{
  \item{a }{The denominator filter coefficients.}
  \item{v }{Variance of the white noise = square of the numerator constant.}
  \item{ref }{Reflection coefficients = coefficients of the lattice
implementation of the filter.}
}
\references{
 Steven M. Kay and Stanley Lawrence Marple Jr.:
   \emph{Spectrum analysis -- a modern perspective},
   Proceedings of the IEEE, Vol 69, pp 1380-1419, Nov., 1981

 Octave \url{http://octave.sf.net}
}
\author{ Original Octave version by Paul Kienzle
  \email{pkienzle@users.sf.net}. Conversion to R by Sebastian Krey
\email{krey@statistik.tu-dortmund.de}. }
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
\keyword{ levinson }
\keyword{ durbin }
\keyword{ recursion }
