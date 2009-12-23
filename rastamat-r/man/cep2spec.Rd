\name{cep2spec}
\alias{cep2spec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cep2spec(cep, nfreq = 21, type = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cep}{
%%     ~~Describe \code{cep} here~~
}
  \item{nfreq}{
%%     ~~Describe \code{nfreq} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function(cep, nfreq=21, type=2){

  ncep <- nrow(cep)
  ncol <- ncol(cep)

  # Make the DCT matrix
  dctm  <- matrix(rep(0, ncep*nfreq), ncol=col)
  idctm <- t(dctm)

  if ((type == 2) || (type == 3)) {
    # this is the orthogonal one, so inv matrix is same as fwd matrix
    for (i in 1:ncep) {
      dctm[i,] <- cos((i-1) * seq(1, (2*nfreq-1), 2) / (2*nfreq) * pi) * sqrt(2/nfreq)
    }
    if (type == 2) {
      # make it unitary! (but not for HTK type 3)
      dctm[1,] <- dctm[1,]/sqrt(2)
    } else {
      dctm[1,] <- dctm[1,]/2
    }
    idctm <- t(Conj(dctm))
  } else if (type == 4) {
    # type 1 with implicit repetition of first, last bins
    # so all we do is reconstruct the middle nfreq rows of an nfreq+2 row idctm
    for (i in 1:ncep) {
      # 2x to compensate for fact that only getting +ve freq half
      idctm[,i] <- 2*cos((i-1) * t(1:nfreq) / (nfreq+1) * pi)
    }
    # fixup 'non-repeated' basis fns 
    idctm[, c(1, ncep)] <- idctm[, c(1, ncep)]/2
  } else { 
    # dpwe type 1 - idft of cosine terms
    for (i in 1:ncep) {
      # 2x to compensate for fact that only getting +ve freq half
      idctm[,i] <- 2*cos((i-1) * t(0:(nfreq-1)) / (nfreq-1) * pi)
    }
    # fixup 'non-repeated' basis fns 
    idctm[, c(1, ncep)] <- idctm[, c(1, ncep)]/2
  }

  spec <- exp(idctm*cep)

  return(list(spec=spec, idctm=idctm))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
