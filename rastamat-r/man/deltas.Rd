\name{deltas}
\alias{deltas}
\title{ Calculate delta features }
\description{
Calculate the deltas (derivatives) of a sequence of features using a simple
linear slope.
}
\usage{
deltas(x, w = 9)
}
\arguments{
  \item{x}{ Matrix of features. Every column represents one point in time
(time frame). Each row is filtered separately. }
  \item{w}{ Window width, has the be odd. }
}
\details{ This functiion mirrors the delta calculation performed in HTKs 'feacalc'.}
\value{
  Returns a matrix of the delta features.
}
\references{Daniel P. W. Ellis:
\url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/}, HTK }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
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
\keyword{ deltas }
