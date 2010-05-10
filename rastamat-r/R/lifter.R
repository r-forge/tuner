# This code is based on the Matlab implementations of PLP and Rasta
# feature calculations by Daniel P. W. Ellis of Columbia University /
# International Computer Science Institute.  For more details, see:
# http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/

lifter <- function(x, lift=0.6, inv=FALSE){

    if(!(is.numeric(x) && is.matrix(x)))
      stop("'x' has to be a numeric matrix")

    ncep <- nrow(x)

    if(lift == 0){
        y <- x
    } else {
    if(lift > 0){
        liftwts <- c(1, (1:(ncep-1))^lift)
    } else
    # HTK liftering
    if(lift < 0){
        L <- -lift
        if(L - as.integer(L) == 0){
            liftwts <- c(1, (1+ L/2 * sin( (1:(ncep-1)) * pi/L)))
        } else {
            stop("HTK liftering value must be integer!")
        }
    }

    if(inv){
        liftwts <- 1/liftwts
    }
    
    y <- diag(liftwts) %*% x
  }
  return(y)
}

