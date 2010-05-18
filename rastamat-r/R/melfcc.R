# This code is based on the Matlab implementations of PLP and Rasta
# feature calculations by Daniel P. W. Ellis of Columbia University /
# International Computer Science Institute.  For more details, see:
# http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/

melfcc <- function(samples, sr=samples@samp.rate, wintime=0.025, hoptime=0.010,
            numcep=13, lifterexp=0.6 HTKlifter=FALSE,
            sumpower=TRUE, preemph=0.97, dither=FALSE,
            minfreq=0, maxfreq=sr/2, nbands=40, bwidth=1.0, dcttype="t2",
            fbtype="mel", usecmp=FALSE, modelorder=NULL, spec.out=FALSE){

    if(!is(samples, "Wave")) 
        stop("'samples' needs to be of class 'Wave'")
    validObject(samples)

    if(samples@stereo) 
        stop("Stereo processing not yet implemented...")

    if(!is.null(modelorder) && !(modelorder==as.integer(modelorder) && modelorder > 0))
        stop("'modelorder' has to be a non-negative integer or NULL")

    if(modelorder > 0 && numcep > modelorder+1)
        stop("No. of cepstra can't be larger than 'modelorder+1'")

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
    cepstra <- lifter(cepstra, lifterexp, HTK=HTKlifter)

    if(spec.out){
      res <- (list(cepstra=cepstra, aspectrum=aspectrum, pspectrum=pspectrum,
              lpcas=lpcas))
    } else {
      res <- cepstra
    }
    return(res)
}
