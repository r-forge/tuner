\name{invpostaud}
\alias{invpostaud}
%- Also NEED an '\alias' for EACH other topic documented here.
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(y, fmax, fbtype = "bark", broaden = 0){

nbands  <- nrow(y)
nframes <- ncol(y)

if (fbtype == "bark") {
  bandcfhz <- bark2hz(seq(0, hz2bark(fmax), length.out=nbands))
  } else if (fbtype == "mel") {
  bandcfhz <- mel2hz(seq(0, hz2mel(fmax), length.out=nbands))
  } else if ((fbtype == "htkmel") || (fbtype == "fcmel")) {
  bandcfhz <- mel2hz(seq(0, hz2mel(fmax,1), length.out=nbands),1)
  } else {
  stop("unknown fbtype ", fbtype)
  }

# Remove extremal bands (the ones that got duplicated)
bandcfhz <- bandcfhz[(1+broaden):(nbands-broaden)]

# Hynek's magic equal-loudness-curve formula
fsq  <- bandcfhz^2
ftmp <- fsq + 1.6e5
eql  <- ((fsq/ftmp)^2) * ((fsq + 1.44e6)/(fsq + 9.61e6))

# cube expand
x <- y ^ (1/.33)

# squash the zero in the eql curve
if (eql[1] == 0) {  # or maybe always
  eql[1]           <- eql[2]
  eql[length(eql)] <- eql[length(eql)-1]
  }

# weight the critical bands
x <- x[(1+broaden):(nbands-broaden),] / kronecker(t(rep(1, nframes)), t(Conj(eql)))

  return(list(x=x, eql=eql))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ loudness }
\keyword{ compression }
\keyword{ inverse }
