\name{rastaplp}
\alias{rastaplp}
\title{RASTA}
\description{ log rasta with fixed parameters }
\usage{
rastaplp(samples, sr = 8000, dorasta = 1, modelorder = 8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{samples}{ Vector of samples.}
  \item{sr}{ Sampling rate of samples.}
  \item{dorasta}{}
  \item{modelorder}{}
}
\value{
  \item{cepstra }{Matrix of resulting cepstra.}
  \item{spectra }{Matrix of resulting spectra.}
  \item{pspectrum }{Matrix of resulting power spectrum.}
  \item{lpcas }{Matrix of the normalized AR coefficients.}
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }
\seealso{ \code{\link{powspec}}, \code{\link{audpec}}, \code{\link{aspectrum}}, \code{\link{rastafilt}}, \code{\link{dolpc}}, \code{\link{lpc2spec}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(samples, sr = samples@samp.rate, dorasta = 1, modelorder = 8) {

  # add miniscule amount of noise
  # samples <- samples + rnorm(length(samples)) * 0.0001

  # first compute power spectrum
  pspectrum <- powspec(samples@left, sr)

  # next group to critical bands
  aspectrum <- audspec(pspectrum, sr)$aspectrum
  nbands <- nrow(aspectrum)

  if (dorasta != 0) {

    # put in log domain
    nl_aspectrum <- log(aspectrum)

    # next do rasta filtering
    ras_nl_aspectrum <- rastafilt(nl_aspectrum)

    # do inverse log
    aspectrum <- exp(ras_nl_aspectrum) # Fehler ~ 1\%

  }

  # do final auditory compressions
  postspectrum <- postaud(aspectrum, sr)$y

  if (modelorder > 0) {

    # LPC analysis 
    lpcas <- dolpc(postspectrum, modelorder)

    # convert lpc to cepstra
    cepstra <- lpc2cep(lpcas, modelorder+1)

    # .. or to spectra
    temp <- lpc2spec(lpcas, nbands)
    spectra <- temp$features
    Fout <- temp$Fout
    Mout <- temp$Mout

  } else {
  
    # No LPC smoothing of spectrum
    spectra <- postspectrum
    cepstra <- spec2cep(spectra)
  
  }
  cepstra <- lifter(cepstra, 0.6)

  return(list(cepstra=cepstra, spectra=spectra, pspectrum=pspectrum, lpcas=lpcas, Fout=Fout, Mout=Mout))

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spectrum }
\keyword{ rasta }
\keyword{ cepstrum }
