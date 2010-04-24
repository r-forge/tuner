\name{invpowspec}
\alias{invpowspec}
\title{ Inverse Powerspectrum }
\description{ Restore the audio signal out of a powerspectrum computed of it. }
\usage{
invpowspec(y, sr = 8000, wintime = 0.025, steptime = 0.01)
}
\arguments{
  \item{y}{ Matrix, where each column represents a power spectrum for a given frame and each row represents a frequency.}
  \item{sr}{ Sampling rate of the signal. }
  \item{wintime}{ Window length in sec. }
  \item{steptime}{ Step between successive windows in sec. }
}
\value{Vector of samples representing the restored signal.}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }
\seealso{ \code{\link{powspec}}, \code{\link{specgram}}, \code{\link{ispecgram}} }
\examples{
}
\concept{ powerspectrum }
\concept{ inverse }
\concept{ frequency }
