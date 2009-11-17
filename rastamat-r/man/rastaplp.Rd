\name{rastaplp}
\alias{rastaplp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rastaplp(samples, sr = 8000, dorasta = 1, modelorder = 8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{samples}{
%%     ~~Describe \code{samples} here~~
}
  \item{sr}{
%%     ~~Describe \code{sr} here~~
}
  \item{dorasta}{
%%     ~~Describe \code{dorasta} here~~
}
  \item{modelorder}{
%%     ~~Describe \code{modelorder} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(samples, sr = 8000, dorasta = 1, modelorder = 8) {

  # add miniscule amount of noise
  # samples <- samples + rnorm(length(samples)) * 0.0001

  # first compute power spectrum
  pspectrum <- powspec(samples, sr)

  # next group to critical bands
  aspectrum <- audspec(pspectrum, sr)$aspectrum
  nbands <- nrow(aspectrum)

  if (dorasta != 0) {

    # put in log domain
    nl_aspectrum <- log(aspectrum)

    # next do rasta filtering
    ras_nl_aspectrum <- rastafilt(nl_aspectrum)

    # do inverse log
    aspectrum <- exp(ras_nl_aspectrum)

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
    F <- temp$F
    M <- temp$M

  } else {
  
    # No LPC smoothing of spectrum
    spectra <- postspectrum
    cepstra <- spec2cep(spectra)
  
  }

  return(list(cepstra=cepstra, spectra=spectra, pspectrum=pspectrum, lpcas=lpcas, F=F, M=M))

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
