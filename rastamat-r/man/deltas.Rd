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
\details{ This functiion mirrors the delta calculation performed in HTKs \sQuote{feacalc}.}
\value{
  Returns a matrix of the delta features.
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/}, HTK }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\examples{
}
\concept{ deltas }
