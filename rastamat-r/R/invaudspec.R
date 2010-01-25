# Not Yet Tested
invaudspec <- function(aspectrum, sr = 16000, nfft = 512, fbtype = "bark", 
    minfreq = 0, maxfreq = -1, sumpower = TRUE, bdwidth = 1){

  if (maxfreq == -1) {
    maxfreq <- sr/2
  }

  nfilts  <- nrow(aspectrum)
  nframes <- ncol(aspectrum)

  if (fbtype == "bark") {
    wts <- fft2barkmx(nfft, sr, nfilts, bwidth, minfreq, maxfreq)
  } else if (fbtype == "mel") {
    wts <- fft2melmx(nfft, sr, nfilts, bwidth, minfreq, maxfreq)
  } else if (fbtype == "htkmel") {
    wts <- fft2melmx(nfft, sr, nfilts, bwidth, minfreq, maxfreq, 1, 1)
  } else if (fbtype == "fcmel") {
    wts <- fft2melmx(nfft, sr, nfilts, bwidth, minfreq, maxfreq, 1)
  } else {
    stop("fbtype", fbtype, "not recognized")
  }
 
  # Cut off 2nd half
  wts <- wts[,1:((nfft/2)+1)]

  # Just transpose, fix up 
  ww   <- t(wts)%*%wts
  iwts <- t(wts)/kronecker(t(rep(1, nfilts)), t(max(mean(diag(ww))/100, sum(ww))))

  # Apply weights
  if (sumpower) {
    spec <- iwts %*% aspectrum
  } else {
    spec <- (iwts %*% sqrt(aspectrum))^2
  }

  return(list(spec=spec, wts=wts, iwts=iwts))
}
