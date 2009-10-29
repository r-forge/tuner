lpc2cep <- function(a, nout=NULL){
    arow <- nrow(a)
    acol <- ncol(a)

    order <- arow - 1

    if(is.null(nout)){
        nout <- order + 1
    }

    mc <- matrix(0, nrow=nout, ncol=acol)

    # Code copied from HSigP.c: LPC2Cepstrum

    # First cep is log(Error) from Durbin
    mc[1,] = -log(a[1,])

    # Renormalize lpc A coeffs
    a <- a / matrix(rep(a[1,], arow), nrow=arow, byrow=TRUE)

    for(n in 2:nout){
        s <- 0
        for(m in 2:n){
            s <- s + (n-m) * a[m,] * mc[n-m+1,]
        }
        mc[n,] <- -(a[n,] + s/(n-1))
    }
    features <- mc

    return(features)
}

