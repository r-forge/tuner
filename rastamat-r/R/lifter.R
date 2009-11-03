lifter <- function(x, lift=0.6, inv=FALSE){
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

