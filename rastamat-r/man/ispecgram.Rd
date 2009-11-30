\name{ispecgram}
\alias{ispecgram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ispecgram(d, ftsize = -1, sr = -1, win = -1, nov = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{ftsize}{
%%     ~~Describe \code{ftsize} here~~
}
  \item{sr}{
%%     ~~Describe \code{sr} here~~
}
  \item{win}{
%%     ~~Describe \code{win} here~~
}
  \item{nov}{
%%     ~~Describe \code{nov} here~~
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
function(d, ftsize = -1, sr = -1, win = -1, nov = -1){

  nspec <- nrow(d)
  ncol  <- ncol(d)

  if (ftsize == -1) {
    ftsize <- 2*(nrow(d)-1)
  }
  if (win == -1) {
    win <- ftsize
  }
  if (nov == -1) {
    nov <- ftsize/2
  }

  hop <- win - nov;

  if (nspec != (ftsize/2)+1) {
    warning('number of rows should be ftsize/2+1')
  }

  xlen <- ftsize + (ncol-1) * hop
  x <- rep(0, xlen)

  halff <- ftsize/2  # midpoint of win

  # No reconstruction win (for now...)

  for (c in 1:ncol) {
    ft <- d[,c]
    ft <- c( ft[1:(ftsize/2+1)], Conj(ft[seq(ftsize/2, 2, -1)]) )

    if (max(Im(fft(ft, inverse=TRUE))) > 1e-5) {
      warning('imag oflow')
    }
  
    px <- Re(fft(ft, inverse=TRUE))  # no shift in specgram
  
    b <- (c-1)*hop
    x[b+(1:ftsize)] <- x[b+(1:ftsize)] + px
  }

  x <- x * win/ftsize  # scale amplitude

  return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
