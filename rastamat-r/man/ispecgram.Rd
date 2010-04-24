\name{ispecgram}
\alias{ispecgram}
\title{ Inverse spectrogram }
\description{ Inversion of \code{\link{specgram}}. Restore signal from spectrogram.}
\usage{ispecgram(d, ftsize = -1, win = -1, nov = -1)}
\arguments{
  \item{d}{}
  \item{ftsize}{}
  \item{win}{}
  \item{nov}{}
}
\value{ Vector of samples. }
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }

\seealso{ \code{\code{specgram}}, \code{\link{fft}} }
\examples{
}
\concept{ spectrogram }
\concept{ inverse }
