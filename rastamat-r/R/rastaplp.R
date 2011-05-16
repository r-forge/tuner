# This code is based on the Matlab implementations of PLP and Rasta
# feature calculations by Daniel P. W. Ellis of Columbia University /
# International Computer Science Institute.  For more details, see:
# http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/

rastaplp <- function(samples, sr = samples@samp.rate, dorasta = TRUE,
            modelorder= 8, wintime=0.025, hoptime=0.010,
            lifterexp=0.6, HTKlifter=FALSE,
            sumpower=TRUE, dither=TRUE,
            minfreq=0, maxfreq=sr/2, nbands=ceiling(hz2bark(sr/2))+1, bwidth=1.0,
            fbtype="bark", spec_out=FALSE, frames_in_rows=TRUE) {

  if(!is(samples, "Wave")) 
      stop("'samples' needs to be of class 'Wave'")
  validObject(samples)

  if(samples@stereo) 
      stop("Stereo processing not yet implemented...")

  if(!is.null(modelorder) && !(modelorder==as.integer(modelorder) && modelorder > 0))
      stop("'modelorder' has to be a non-negative integer or NULL")

  # add miniscule amount of noise
  # samples <- samples + rnorm(length(samples)) * 0.0001

  # first compute power spectrum
  pspectrum <- powspec(samples@left, sr, wintime, hoptime, dither)

  # next group to critical bands
  aspectrum <- audspec(pspectrum, sr, nbands, fbtype, minfreq, maxfreq,
                    sumpower, bwidth)$aspectrum
  # nbands <- nrow(aspectrum)

  if (dorasta) {

    # put in log domain
    nl_aspectrum <- log(aspectrum)

    # next do rasta filtering
    ras_nl_aspectrum <- rastafilt(nl_aspectrum)

    # do inverse log
    aspectrum <- exp(ras_nl_aspectrum) # Fehler ~ 1%

  }

  # do final auditory compressions
  postspectrum <- postaud(aspectrum, maxfreq, fbtype)$y

  if (modelorder > 0) {

    # LPC analysis 
    lpcas <- dolpc(postspectrum, modelorder)

    # convert lpc to cepstra
    cepstra <- lpc2cep(lpcas, modelorder+1)

    # .. or to spectra
    temp <- lpc2spec(lpcas, nbands)
    spectra <- temp$features
    Fout <- temp$Fout
    Mout <- temp$Mout

  } else {
  
    # No LPC smoothing of spectrum
    spectra <- postspectrum
    cepstra <- spec2cep(spectra)
  
  }
  cepstra <- lifter(cepstra, lifterexp, HTK=HTKlifter)

  if(spec_out){
    if(frames_in_rows){
      res <- list(cepstra=t(cepstra), spectra=t(spectra),
          pspectrum=t(pspectrum), lpcas=t(lpcas), Fout=t(Fout), Mout=t(Mout)) 
    } else {
      res <- list(cepstra=cepstra, spectra=spectra, pspectrum=pspectrum, lpcas=lpcas, Fout=Fout, Mout=Mout) 
    }
  } else {
    if(frames_in_rows){
      res <- list(cepstra=t(cepstra), lpcas=t(lpcas), Fout=t(Fout), Mout=t(Mout)) 
    } else {
      res <- list(cepstra=cepstra, lpcas=lpcas, Fout=Fout, Mout=Mout) 
    }
  }
  return(res)
}
