\name{lpc2spec}
\alias{lpc2spec}
\title{ LPC to spectra conversion }
\description{ Convert the LPC coefficients back into spectra. }
\usage{
lpc2spec(lpcas, nout = 17)
}
\arguments{
  \item{lpcas}{ matrix of LPC coefficients for each time frame }
  \item{nout}{ number of freq channels }
}
\value{
  \item{features }{frequence features for each time frame}
  \item{Fout }{}
  \item{Mout }{}
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Stefan Tewes \email{stefan.tewes@tu-dortmund.de} }
\seealso{ \code{\link{lpc2cep}}, \code{\link{spec2cep}}, \code{\link{cep2spec}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(lpcas, nout = 17) {

  rows <- nrow(lpcas)
  cols <- ncol(lpcas)
  order <- rows - 1

  gg <- lpcas[1,]
  aa <- t(t(lpcas) / gg)

  # Calculate the actual z-plane polyvals: nout points around unit circle
  zz <- exp( (-1i * (0:(nout-1)) * pi / (nout-1)) \%*\% t(0:order))

  # Actual polyvals, in power (mag^2)
  features <- t(t((1/abs(zz \%*\% aa))^2) / gg)

  F <- matrix(0, cols, floor(rows/2)) 
  M <- F

  for (c in 1:cols) {
    aaa <- aa[,c]
    rr <- polyroot(rev(t(aaa)))
    ff <- Arg(t(rr))

    zz <- exp(1i * t(ff) \%*\% (0:(length(aaa)-1)))
    mags <- sqrt(t( ((1/abs(zz\%*\%aaa))^2) / gg[c] ))
  
    ix <- order(ff)
    keep <- ff[ix] > 0
    ix <- ix[keep]
    F[c,1:length(ix)] <- ff[ix]
    M[c,1:length(ix)] <- mags[ix]
  }

  return(list(features=features, F=F, M=M))

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ lpc }
\keyword{ spectra }
\keyword{ conversion }
