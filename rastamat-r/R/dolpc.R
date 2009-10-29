dolpc <- function(x, modelorder=8){
    nbands <- nrow(x)

    # px <- planFFT(2*nbands-2)
    # Calculate autocorrelation 
    r <- apply(rbind(x, x[seq(nbands-1, 2, -1),]), 2,
            function(y) Re(fft(y, inverse=TRUE))/length(y))
            # function(y) Re(IFFT(y, plan=px)))
    # First half only
    r <- r[1:nbands,]

    # Find LPC coeffs by Levinson-Durbin
    levcoef <- levinson(r, modelorder)

    # Normalize each poly by gain
    y <- t(levcoef$a) / matrix(rep(levcoef$v, modelorder+1), nrow=modelorder+1,
            byrow=TRUE)

    return(y)
}

