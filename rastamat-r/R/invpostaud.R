# This code is based on the Matlab implementations of PLP and Rasta
# feature calculations by Daniel P. W. Ellis of Columbia University /
# International Computer Science Institute.  For more details, see:
# http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/

# Not Yet Tested
invpostaud <- function(y, fmax, fbtype = c("bark", "mel", "htkmel", "fcmel"), broaden = 0){

fbtype <- match.arg(fbtype)

nbands  <- nrow(y)
nframes <- ncol(y)

bandcfhz <- switch(fbtype,
    "bark" = bark2hz(z=seq(0, hz2bark(f=fmax), length.out=nbands)),
    "mel" = mel2hz(z=seq(0, hz2mel(f=fmax), length.out=nbands)),
    "htkmel" = mel2hz(z=seq(0, hz2mel(f=fmax, htk=TRUE), length.out=nbands), htk=TRUE),
    "fcmel" = mel2hz(z=seq(0, hz2mel(f=fmax, htk=TRUE), length.out=nbands), htk=TRUE),
)


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
x <- x[(1+broaden):(nbands-broaden),,drop=FALSE] / kronecker(t(rep(1, nframes)), t(Conj(eql)))

  return(list(x=x, eql=eql))
}
