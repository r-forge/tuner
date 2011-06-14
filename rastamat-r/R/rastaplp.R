# This code is based on the Matlab implementations of PLP and Rasta
# feature calculations by Daniel P. W. Ellis of Columbia University /
# International Computer Science Institute.  For more details, see:
# http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/

rastaplp <- function(samples, sr = samples@samp.rate, dorasta = TRUE,
            modelorder= 8, wintime=0.025, hoptime=0.010,
            lifterexp=0.6, htklifter=FALSE,
            sumpower=TRUE, dither=TRUE,
            minfreq=0, maxfreq=sr/2, nbands=ceiling(hz2bark(sr/2))+1, bwidth=1.0,
            fbtype=c("bark", "mel", "htkmel", "fcmel"),
            spec_out=FALSE, frames_in_rows=TRUE, ...) {

  fbtype <- match.arg(fbtype)

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
  pspectrum <- powspec(x=samples@left, sr=sr, wintime=wintime, steptime=hoptime,
      dither=dither)

  # next group to critical bands
  aspectrum <- audspec(pspectrum=pspectrum, sr=sr, nfilts=nbands, fbtype=fbtype,
      minfreq=minfreq, maxfreq=maxfreq, sumpower=sumpower, bwidth=bwidth)$aspectrum
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
  postspectrum <- postaud(x=aspectrum, fmax=maxfreq, fbtype=fbtype)$y

  if (modelorder > 0) {

    # LPC analysis 
    lpcas <- dolpc(x=postspectrum, modelorder=modelorder)

    # convert lpc to cepstra
    cepstra <- lpc2cep(a=lpcas, nout=modelorder+1)

    # .. or to spectra
    temp <- lpc2spec(lpcas=lpcas, nout=nbands)
    spectra <- temp$features
    Fout <- temp$Fout
    Mout <- temp$Mout

  } else {
  
    # No LPC smoothing of spectrum
    spectra <- postspectrum
    cepstra <- spec2cep(spec=spectra, ...)
  
  }
  cepstra <- lifter(x=cepstra, lift=lifterexp, htk=htklifter)

  if(spec_out){
    if(frames_in_rows){
      res <- list(cepstra=t(cepstra), spectra=t(spectra),
          pspectrum=t(pspectrum), lpcas=t(lpcas), Fout=t(Fout), Mout=t(Mout)) 
    } else {
      res <- list(cepstra=cepstra, spectra=spectra, pspectrum=pspectrum, lpcas=lpcas, Fout=Fout, Mout=Mout) 
    }
  } else {
    if(frames_in_rows){
      res <- list(cepstra=t(cepstra), lpcas=t(lpcas)) 
    } else {
      res <- list(cepstra=cepstra, lpcas=lpcas) 
    }
  }
  return(res)
}
