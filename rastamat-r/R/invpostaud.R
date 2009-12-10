# Not Yet Tested
invpostaud <- function(y, fmax, fbtype = "bark", broaden = 0){

nbands  <- nrow(y)
nframes <- ncol(y)

if (fbtype == "bark") {
  bandcfhz <- bark2hz(seq(0, hz2bark(fmax), length.out=nbands))
} else if (fbtype == "mel") {
  bandcfhz <- mel2hz(seq(0, hz2mel(fmax), length.out=nbands))
} else if ((fbtype == "htkmel") || (fbtype == "fcmel")) {
  bandcfhz <- mel2hz(seq(0, hz2mel(fmax,1), length.out=nbands),1)
} else {
  stop("unknown fbtype ", fbtype)
}

# Remove extremal bands (the ones that got duplicated)
bandcfhz <- bandcfhz[(1+broaden):(nbands-broaden)]

# Hynek's magic equal-loudness-curve formula
fsq  <- bandcfhz^2
ftmp <- fsq + 1.6e5
eql  <- ((fsq/ftmp)^2) * ((fsq + 1.44e6)/(fsq + 9.61e6))

# cube expand
x <- y ^ (1/.33)

# squash the zero in the eql curve
if (eql[1] == 0) {  # or maybe always
  eql[1]           <- eql[2]
  eql[length(eql)] <- eql[length(eql)-1]
}

# weight the critical bands
x <- x[(1+broaden):(nbands-broaden),] / kronecker(t(rep(1, nframes)), t(Conj(eql)))

  return(list(x=x, eql=eql))
}
