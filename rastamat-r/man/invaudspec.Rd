\name{invaudspec}
\alias{invaudspec}
\title{ Inverse frequency band conversion }
\description{ Invert the effects of \code{\link{audspec}}. }
\usage{
invaudspec(aspectrum, sr = 16000, nfft = 512, fbtype = "bark", minfreq = 0, 
    maxfreq = -1, sumpower = TRUE, bdwidth = 1)
}
\arguments{
  \item{aspectrum}{Matrix with the auditory spectrum of each time frame in its columns.}
  \item{sr}{ Samplerate of the original recording.}
  \item{nfft}{}
  \item{fbtype}{ Used auditory frequency scale.}
  \item{minfreq}{ Lowest frequency.}
  \item{maxfreq}{ Highest frequency. }
  \item{sumpower}{ If \code{TRUE} the frequency scale transformation is based on the
auditory spectrum, if \code{FALSE} on its squareroot (absolute value of the spectrum) and
squared afterwards.
  \item{bdwidth}{ Modify the width of the frequency bands.}
}
\value{
  \item{spec }{Matrix with the powerspectrum of each time frame in its columns.}
  \item{wts }{}
  \item{iwts }{}
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }
\seealso{\code{\link{audspec}}}
\examples{
}
\concept{ frequency }
\concept{ conversion }
\concept{ mel }
\concept{ bark }
