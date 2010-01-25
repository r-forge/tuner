\name{lifter}
\alias{lifter}
\title{ Liftering of cepstra }
\description{
   Apply liftering to a matrix of cepstra.
}
\usage{
lifter(x, lift = 0.6, inv = FALSE)
}
\arguments{
  \item{x}{ Matrix of cepstra, one sample/time frame per column. }
  \item{lift}{ Liftering exponent, positive real number or a negative integer. }
  \item{inv}{ Invert the liftering (undo a previous liftering). }
}
\details{
 If 'lift' is positive, then perform \eqn{x i^lift}, \eqn{i = 1, ldots, nrow(x)}
liftering. If 'lift' is a negative integer, then perform HTK-style sin-curve
liftering with length 'lift'.
}
\value{
Matrix of the liftered cepstra.
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
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
\keyword{ liftering }
