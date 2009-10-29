\name{spec2cep}
\alias{spec2cep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~

% [cep,dctm] = spec2cep(spec, ncep, type)
%     Calculate cepstra from spectral samples (in columns of spec)
%     Return ncep cepstral rows (defaults to 9)
%     This one does type II dct, or type I if type is specified as 1
%     dctm returns the DCT matrix that spec was multiplied by to give cep.
% 2005-04-19 dpwe@ee.columbia.edu  for mfcc_dpwe
}
\usage{
spec2cep(spec, ncep = 13, type = c("t2", "t1", "t3", "t4"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{ ~~Describe \code{spec} here~~ }
  \item{ncep}{ ~~Describe \code{ncep} here~~ }
  \item{type}{ ~~Describe \code{type} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (spec, ncep = 13, type = c("t2", "t1", "t3", "t4")) 
{
    dctm23 <- function(spec, ncep) {
        srow <- nrow(spec)
        return(cos((seq(ncep) - 1) * matrix(rep(seq(1, (2 * srow - 
            1), 2), each = ncep), nrow = ncep)/(2 * srow) * pi) * 
            sqrt(2/srow))
    }
    dctm2 <- function(spec, ncep) {
        dctm <- dctm23(spec, ncep)
        dctm[1, ] <- dctm[1, ]/sqrt(2)
        return(dctm)
    }
    dctm4 <- function(spec, ncep) {
        srow <- nrow(spec)
        dctm <- cos((seq(ncep) - 1) * matrix(rep(seq(srow), each = ncep), 
            nrow = ncep)/(srow + 1) * pi) * 2
        dctm[, 1] <- dctm[, 1] + 1
        dctm[, srow] <- dctm[, srow] + ((-1)^(seq(ncep) - 1))
        dctm <- dctm/(2 * (srow + 1))
        return(dctm)
    }
    dctm1 <- function(spec, ncep) {
        srow <- nrow(spec)
        dctm <- cos((seq(ncep) - 1) * matrix(rep(0:(srow - 1), 
            each = ncep), nrow = ncep)/(srow - 1) * pi) * 2/(2 * 
            (srow - 1))
        dctm[, c(1, srow)] <- dctm[, c(1, srow)]/2
        return(dctm)
    }
    type <- match.arg(type)
    dctm <- switch(type, t1 = dctm1(spec, ncep), t2 = dctm2(spec, 
        ncep), t3 = dctm23(spec, ncep), t4 = dctm4(spec, ncep))
    cep <- dctm \%*\% log(spec)
    return(list(cep = cep, dctm = dctm))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
