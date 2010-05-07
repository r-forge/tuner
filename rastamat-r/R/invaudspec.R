# This code is based on the Matlab implementations of PLP and Rasta
# feature calculations by Daniel P. W. Ellis of Columbia University /
# International Computer Science Institute.  For more details, see:
# http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/

# Not Yet Tested
invaudspec <- function(aspectrum, sr = 16000, nfft = 512, fbtype = c("bark", "mel", "htkmel", "fcmel"), 
    minfreq = 0, maxfreq = sr/2, sumpower = TRUE, bdwidth = 1){

  nfilts  <- nrow(aspectrum)
  nframes <- ncol(aspectrum)

  fbtype <- match.arg(fbtype)
  wts <- switch(fbtype,
          bark = fft2barkmx(nfft, sr, nfilts, bdwidth, minfreq, maxfreq),
          mel  = fft2melmx(nfft, sr, nfilts, bdwidth, minfreq, maxfreq),
          htkmel = fft2melmx(nfft, sr, nfilts, bdwidth, minfreq, maxfreq, TRUE, TRUE),
          fcmel = fft2melmx(nfft, sr, nfilts, bdwidth, minfreq, maxfreq, TRUE)
  )
 
  # Cut off 2nd half
  wts <- wts[,1:((nfft/2)+1),drop=FALSE]

  # Just transpose, fix up 
  ww   <- t(wts)%*%wts
  iwts <- t(wts)/kronecker(t(rep(1, nfilts)), pmax(mean(diag(ww))/100, colSums(ww)))

  # Apply weights
  if (sumpower) {
    spec <- iwts %*% aspectrum
  } else {
    spec <- (iwts %*% sqrt(aspectrum))^2
  }

  return(list(spec=spec, wts=wts, iwts=iwts))
}
