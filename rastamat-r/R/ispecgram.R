# Not Yet Tested
ispecgram <- function(d, ftsize = -1, sr = -1, win = -1, nov = -1){

  nspec <- nrow(d)
  ncol  <- ncol(d)

  if (ftsize == -1) {
    ftsize <- 2*(nrow(d)-1)
  }
  if (win == -1) {
    win <- ftsize
  }
  if (nov == -1) {
    nov <- ftsize/2
  }

  hop <- win - nov;

  if (nspec != (ftsize/2)+1) {
    warning('number of rows should be ftsize/2+1')
  }

  xlen <- ftsize + (ncol-1) * hop
  x <- rep(0, xlen)

  halff <- ftsize/2  # midpoint of win

  # No reconstruction win (for now...)

  for (c in 1:ncol) {
    ft <- d[,c]
    ft <- c( ft[1:(ftsize/2+1)], Conj(ft[seq(ftsize/2, 2, -1)]) )

    if (max(Im(fft(ft, inverse=TRUE))) > 1e-5) {
      warning('imag oflow')
    }
  
    px <- Re(fft(ft, inverse=TRUE))  # no shift in specgram
  
    b <- (c-1)*hop
    x[b+(1:ftsize)] <- x[b+(1:ftsize)] + px
  }

  x <- x * win/ftsize  # scale amplitude

  return(x)
}



