\name{invpostaud}
\alias{invpostaud}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
invpostaud(y, fmax, fbtype = "bark", broaden = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{fmax}{
%%     ~~Describe \code{fmax} here~~
}
  \item{fbtype}{
%%     ~~Describe \code{fbtype} here~~
}
  \item{broaden}{
%%     ~~Describe \code{broaden} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(y, fmax, fbtype = "bark", broaden = 0){

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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
