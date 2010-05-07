# This code is based on the Matlab implementations of PLP and Rasta
# feature calculations by Daniel P. W. Ellis of Columbia University /
# International Computer Science Institute.  For more details, see:
# http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/

# Not Yet Tested
invpowspec <- function(y, sr = 8000, wintime = 0.025, steptime = 0.010){

  nrow <- nrow(y)
  ncol <- ncol(y)

  winpts <- round(wintime*sr)
  steppts <- round(steptime*sr)

  NFFT <- 2^(ceiling(log(winpts)/log(2)))

  if (NFFT != 2*(nrow-1)) {
    warning("Inferred FFT size doesn't match specgram")
  }

  NOVERLAP <- winpts - steppts
  SAMPRATE <- sr

  # Values coming out of rasta treat samples as integers, 
  # not range -1..1, hence scale up here to match (approx)
  # y = abs(specgram(x*32768,NFFT,SAMPRATE,WINDOW,NOVERLAP))^2

  xlen <- winpts + steppts*(ncol - 1)

  r <- t(rnorm(xlen))

  R <- specgram(r/32768/12, NFFT, SAMPRATE, winpts, NOVERLAP)$S
  R = R * sqrt(y)
  x = ispecgram(R, NFFT, SAMPRATE, winpts, NOVERLAP)

  return(x)
}

