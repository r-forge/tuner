\name{invpostaud}
\alias{invpostaud}
\title{ Inversion of equal loudness compression.}
\description{ Invert the equal lodness compression effect of \code{\link{postaud}}.}
\usage{
invpostaud(y, fmax, fbtype = "bark", broaden = 0)
}
\arguments{
  \item{y}{ Matrix of spectra.}
  \item{fmax}{ Maximum frequency im Hertz.}
  \item{fbtype}{ Auditory frequency scale.}
  \item{broaden}{ Use two additional frequency bands for calculation.}
}
\value{
  \item{x }{Matrix of the per sample/frame (columns) spectra after applying the frequency
dependant loudness equalization and compression. }
  \item{eql }{Vector of the equal loudness curve. }
}
\references{Daniel P. W. Ellis
\url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/}, Hynek
Hermansky }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }
\seealso{\code{\link{postaud}}}
\examples{
}
\concept{ loudness }
\concept{ compression }
\concept{ inverse }
