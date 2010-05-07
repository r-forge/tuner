# This code is based on the Matlab implementations of PLP and Rasta
# feature calculations by Daniel P. W. Ellis of Columbia University /
# International Computer Science Institute.  For more details, see:
# http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/

rastafilt <- function(x) {

  # rasta filter
  numer = -2:2
  numer = -numer / sum(numer * numer)
  denom = -0.94

  # Initialize the state.  This avoids a big spike at the beginning 
  # resulting from the dc offset level in each band.
  # (this is effectively what rasta/rasta_filt.c does).

  y <- t(filter(t(x), numer, sides = 1))
  # replace NA by zero
  y[,1:4] <- 0
  y <- t(filter(t(y), -denom, sides = 1, method = "recursive"))

  return(y)

}
