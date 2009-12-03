lpc2spec <- function(lpcas, nout = 17) {

  rows <- nrow(lpcas)
  cols <- ncol(lpcas)
  morder <- rows - 1

  gg <- lpcas[1,]
  aa <- t(t(lpcas) / gg)

  # Calculate the actual z-plane polyvals: nout points around unit circle
  zz <- exp( (-1i * (0:(nout-1)) * pi / (nout-1)) %*% t(0:morder))

  # Actual polyvals, in power (mag^2)
  features <- t(t((1/abs(zz %*% aa))^2) / gg)

  F <- matrix(0, cols, floor(rows/2)) 
  M <- F

  for (i in 1:cols) {
    aaa <- aa[,i]
    rr <- polyroot(rev(t(aaa)))
    ff <- Arg(t(rr))

    zz <- exp(1i * t(ff) %*% (0:(length(aaa)-1)))
    mags <- sqrt(t( ((1/abs(zz%*%aaa))^2) / gg[i] ))
  
    ix <- order(ff)
    ff[which(sapply(ff, function(x) isTRUE(all.equal(0, x))))] <- 0
    keep <- ff[ix] > 0
    ix <- ix[keep]
    F[i,1:length(ix)] <- ff[ix]
    M[i,1:length(ix)] <- mags[ix]
  }

  return(list(features=features, F=F, M=M))

}
