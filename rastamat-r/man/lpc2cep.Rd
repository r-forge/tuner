\name{lpc2cep}
\alias{lpc2cep}
\title{ LPC to cepstra conversion }
\description{
 Convert the LPC coefficients in each column of 'a'
 into frames of cepstra.
}
\usage{
lpc2cep(a, nout = nrow(a))
}
\arguments{
  \item{a}{ Matrix of LPC coefficients. }
  \item{nout}{ Number of cepstra to produce. }
}
\value{
Matrix of cepstra.
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{ \code{\link{lpc2spec}}, \code{\link{spec2cep}}, \code{\link{cep2spec}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (a, nout = nrow(a)) 
{
    arow <- nrow(a)
    acol <- ncol(a)
    order <- arow - 1
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
\keyword{ lpc }
\keyword{ cepstra }
\keyword{ conversion }
