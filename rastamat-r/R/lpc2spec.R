# This code is based on the Matlab implementations of PLP and Rasta
# feature calculations by Daniel P. W. Ellis of Columbia University /
# International Computer Science Institute.  For more details, see:
# http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/

lpc2spec <- function(lpcas, nout = 17) {

  if(!(is.numeric(lpcas) && is.matrix(lpcas)))
    stop("'lpcas' has to be a numeric matrix")

  if(!(is.integer(nout) && nout > 0))
      stop("'modelorder' has to be a positive integer")

  rows <- nrow(lpcas)
  cols <- ncol(lpcas)
  morder <- rows - 1

  gg <- lpcas[1,]
  aa <- t(t(lpcas) / gg)

  # Calculate the actual z-plane polyvals: nout points around unit circle
  zz <- exp( (-1i * (0:(nout-1)) * pi / (nout-1)) %*% t(0:morder))

  # Actual polyvals, in power (mag^2)
  features <- t(t((1/abs(zz %*% aa))^2) / gg)

  Fout <- matrix(0, cols, floor(rows/2)) 
  Mout <- Fout

  for (c in 1:cols) {
    aaa <- aa[,c]
    rr <- polyroot(rev(t(aaa)))
    ff <- Arg(t(rr))    # R erzeugt PI, wenn MatLab -PI
    ff[sapply(ff, function(x) all.equal(x, pi)) == T] <- -pi # Korrektur

    zz <- exp(1i * t(ff) %*% (0:(length(aaa)-1)))
    mags <- sqrt(t( ((1/abs(zz%*%aaa))^2) / gg[c] ))
  
    ix <- order(ff)
    ff[which(sapply(ff, function(x) isTRUE(all.equal(0, x))))] <- 0
    keep <- ff[ix] > 0
    ix <- ix[keep]
    Fout[c,1:length(ix)] <- ff[ix]
    Mout[c,1:length(ix)] <- mags[ix]
  }

  return(list(features=features, Fout=Fout, Mout=Mout))

}
