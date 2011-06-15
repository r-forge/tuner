spectempevo <- function(samples, sr=samples@samp.rate, wintime=0.032, hoptime=0.004,
            K=16, S=1, preemph=0, dither=FALSE){

    if(!is(samples, "Wave")) 
        stop("'samples' needs to be of class 'Wave'")
    validObject(samples)

    if(samples@stereo) 
        stop("Stereo processing not yet implemented...")

    if(preemph != 0){
        ssamples <- filter(samples@left, filter=c(1, -preemph), method="convolution", sides=1, circular=FALSE)
    } else {
        ssamples <- samples@left
    }

    # Compute FFT power spectrum
    pspectrum <- powspec(x=ssamples, sr=sr, wintime=wintime, steptime=hoptime, dither=dither)
    dpsp <-dim(pspectrum)

    # Cepstra via FFT
    # ps <- planFFT(dpsp[1])
    # cepstra <- apply(S, 2, function(x) IFFT(x, plan=ps))
    # cepstra <- apply(pspectrum, 2, function(x) fft(log(x), inverse=TRUE)/length(x))
    cepstra <- mvfft(log(pspectrum), inverse=TRUE)/matrix(rep(dpsp[1], prod(dpsp)), nrow=dpsp[1])

    lambda <- seq(1, (dpsp[2]-K+1), by=S)
    llambda <- length(lambda)

    # stevo <- array(mapply(function(x,y) abs(fft(cepstra[y,x:(x+K-1)])), rep(lambda, each=dpsp[1]), seq(dpsp[1])), c(K, dpsp[1], llambda))
    # stevo <- array(sapply(lambda, function(x) abs(mvfft(t(cepstra[,x:(x+K-1)])))), c(K, dpsp[1], llambda))
    # stevo <- sapply(lambda, function(x) abs(mvfft(t(cepstra[,x:(x+K-1)]))))
    # dim(stevo) <- c(K, dpsp[1], llambda)
    stevo <- array(numeric(K*dpsp[1]*llambda), c(K, dpsp[1], llambda))
    for(i in seq(lambda)){
      # stevo[,,i] <- apply(cepstra[,lambda[i]:(lambda[i]+K-1)], 1, function(x) abs(fft(x)))
      stevo[,,i] <- abs(mvfft(t(cepstra[,lambda[i]:(lambda[i]+K-1),drop=FALSE])))
    }
    
    return(stevo)
}

