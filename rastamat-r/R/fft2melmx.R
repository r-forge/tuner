# This code is based on the Matlab implementations of PLP and Rasta
# feature calculations by Daniel P. W. Ellis of Columbia University /
# International Computer Science Institute.  For more details, see:
# http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/

fft2melmx <- function(nfft, sr=8000, nfilts=40, width=1.0, minfrq=0, maxfrq=sr/2,
                htkmel=FALSE, constamp=FALSE){
    fftfrqs <- (0:(nfft-1))/nfft * sr

    minmel <- hz2mel(minfrq, htkmel)
    maxmel <- hz2mel(maxfrq, htkmel)
    # Frequency of each FFT bin in Mel
    binfrqs <- mel2hz(minmel + (0:(nfilts+1))/(nfilts+1) * (maxmel-minmel), htkmel)

    binbin <- round(binfrqs/sr * (nfft-1))

    wtscalc <- function(i, binfrqs=binfrqs){
        fs <- binfrqs[i + c(0, 1, 2)]
        # Scale by width
        fs <- fs[2] + width * (fs - fs[2])

        # Calculate slopes
        loslope <- (fftfrqs - fs[1])/(fs[2] - fs[1])
        hislope <- (fs[3] - fftfrqs)/(fs[3] - fs[2])

        return(pmax(0, pmin(loslope, hislope)))
    }
    wts <- t(sapply(seq(nfilts), function(x) wtscalc(x, binfrqs)))

    if(!constamp){
        # Scale to be approx constant E (Slaney-style mel)
        wts <- diag(2/(binfrqs[2+(1:nfilts)] - binfrqs[1:nfilts])) %*% wts
    }

    # Ensure 2nd half of FFT ist zero
#    wts[,(nfft/2 + 1):nfft] <- 0

    return(list(wts=wts, binfrqs=binfrqs))
}

