# This code is based on the Matlab implementations of PLP and Rasta
# feature calculations by Daniel P. W. Ellis of Columbia University /
# International Computer Science Institute.  For more details, see:
# http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/

lifter <- function(x, lift=0.6, inv=FALSE, HTK=FALSE){

  if(!(is.numeric(x) && is.matrix(x)))
    stop("'x' has to be a numeric matrix")

  if(lift < 0)
    stop("'lift' has to be non-negativ")

  if(HTK && !(lift==as.integer(lift)))
    stop("HTK liftering value must be integer!")

  ncep <- nrow(x)

  if(lift == 0){
      y <- x
  } else {
    if(HTK){
      # HTK liftering
      liftwts <- c(1, (1+ lift/2 * sin( (1:(ncep-1)) * pi/lift)))
    } else {
      liftwts <- c(1, (1:(ncep-1))^lift)
    }

    if(inv){
        liftwts <- 1/liftwts
    }
  
    y <- diag(liftwts) %*% x
  }
  return(y)
}

