rastaplp <- function(samples, sr = samples@samp.rate, dorasta = 1, modelorder = 8) {

  # add miniscule amount of noise
  # samples <- samples + rnorm(length(samples)) * 0.0001

  # first compute power spectrum
  pspectrum <- powspec(samples@left, sr)

  # next group to critical bands
  aspectrum <- audspec(pspectrum, sr)$aspectrum
  nbands <- nrow(aspectrum)

  if (dorasta != 0) {

    # put in log domain
    nl_aspectrum <- log(aspectrum)

    # next do rasta filtering
    ras_nl_aspectrum <- rastafilt(nl_aspectrum)

    # do inverse log
    aspectrum <- exp(ras_nl_aspectrum)

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
    F <- temp$F
    M <- temp$M

  } else {
  
    # No LPC smoothing of spectrum
    spectra <- postspectrum
    cepstra <- spec2cep(spectra)
  
  }
  cepstra <- lifter(cepstra, 0.6)

  return(list(cepstra=cepstra, spectra=spectra, pspectrum=pspectrum, lpcas=lpcas, F=F, M=M))

}
