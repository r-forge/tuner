# This code is based on the Matlab implementations of PLP and Rasta
# feature calculations by Daniel P. W. Ellis of Columbia University /
# International Computer Science Institute.  For more details, see:
# http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/

rastaplp <- function(samples, sr = samples@samp.rate, dorasta = TRUE, modelorder = 8) {

  if(!is(samples, "Wave")) 
      stop("'samples' needs to be of class 'Wave'")
  validObject(samples)

  if(samples@stereo) 
      stop("Stereo processing not yet implemented...")

  if(!is.null(modelorder) && !(is.integer(modelorder) && modelorder > 0))
      stop("'modelorder' has to be a non-negative integer or NULL")

  # add miniscule amount of noise
  # samples <- samples + rnorm(length(samples)) * 0.0001

  # first compute power spectrum
  pspectrum <- powspec(samples@left, sr)

  # next group to critical bands
  aspectrum <- audspec(pspectrum, sr)$aspectrum
  nbands <- nrow(aspectrum)

  if (dorasta) {

    # put in log domain
    nl_aspectrum <- log(aspectrum)

    # next do rasta filtering
    ras_nl_aspectrum <- rastafilt(nl_aspectrum)

    # do inverse log
    aspectrum <- exp(ras_nl_aspectrum) # Fehler ~ 1%

  }

  # do final auditory compressions
  postspectrum <- postaud(aspectrum, sr)$y

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
  cepstra <- lifter(cepstra, 0.6)

  return(list(cepstra=cepstra, spectra=spectra, pspectrum=pspectrum, lpcas=lpcas, Fout=Fout, Mout=Mout))

}
