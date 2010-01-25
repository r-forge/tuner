melfcc <- function(samples, sr=samples@samp.rate, wintime=0.025, hoptime=0.010,
            numcep=13, lifterexp=0.6, sumpower=TRUE, preemph=0.97, dither=FALSE,
            minfreq=0, maxfreq=sr/2, nbands=40, bwidth=1.0, dcttype="t2",
            fbtype="mel", usecmp=FALSE, modelorder=0){

    if(preemph != 0){
        ssamples <- filter(samples@left, filter=c(1, -preemph), method="convolution", sides=1, circular=FALSE)
        ssamples[1] <- samples@left[1]
    } else {
        ssamples <- samples@left
    }

    # Compute FFT power spectrum
    pspectrum <- powspec(ssamples, sr, wintime, hoptime, dither)

    # Conversion to Mel/Bark scale
    aspectrum <- audspec(pspectrum, sr, nbands, fbtype, minfreq, maxfreq,
                    sumpower, bwidth)$aspectrum

    # PLP-like weighting and compression
    if(usecmp){
        aspectrum <- postaud(aspectrum, maxfreq, fbtype)$y
    }

    #lpcas <- numeric(0)
    lpcas <- NULL
    if(modelorder > 0){
        if(dcttype != "t1"){
            warning("PLP cepstra are implicitly dcttype 1")
        }

        # LPC/PLP
        lpcas <- dolpc(aspectrum, modelorder)

        # Cepstra out of LPC/PLP
        cepstra <- lpc2cep(lpcas, numcep)
    } else {
        # Cepstra via DCT
        cepstra <- spec2cep(aspectrum, numcep, dcttype)$cep
    }

    # Liftering
    cepstra <- lifter(cepstra, lifterexp)

    return(list(cepstra=cepstra, aspectrum=aspectrum, pspectrum=pspectrum,
            lpcas=lpcas))
}
