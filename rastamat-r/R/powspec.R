powspec <- function(x, sr=8000, wintime=0.025, steptime=0.010, dither=TRUE){
    winpts <- round(wintime * sr)
    steppts <- round(steptime * sr)

    nfft <- 2^(ceiling(log(winpts)/log(2)))
    window <- hamming.window(winpts)
    noverlap <- winpts - steppts

    y <- abs(specgram(x, nfft, sr, window, noverlap)$S)^2

    # Avoid digital zero
    if(dither){
        y <- y + winpts
    }

    return(y)
}

