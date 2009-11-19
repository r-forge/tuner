\name{rastafilt}
\alias{rastafilt}
\title{Using RASTA filter}
\description{
  \code{rastafilt} ist used to filter each frequency band with the RASTA (Relative Spectral Transform) filter.}
\usage{
  rastafilt(x)
}
\arguments{
  \item{x}{a matrix where rows of x are the critical bands and columns of x are the frames.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Matrix of the same type as \code{x} after RASTA filtering.
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
function(x) {

  # rasta filter
  numer = -2:2
  numer = -numer / sum(numer * numer)
  denom = -0.94

  # Initialize the state.  This avoids a big spike at the beginning 
  # resulting from the dc offset level in each band.
  # (this is effectively what rasta/rasta_filt.c does).

  y <- t(filter(t(x), numer, sides = 1))
  # replace NA by zero
  y[,1:4] <- 0
  y <- t(filter(t(y), -denom, sides = 1, method = "recursive"))

  return(y)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
