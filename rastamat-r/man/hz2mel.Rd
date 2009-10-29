\name{hz2mel}
\alias{hz2mel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hz2mel(f, htk = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{ ~~Describe \code{f} here~~ }
  \item{htk}{ ~~Describe \code{htk} here~~ }
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
function (f, htk = FALSE) 
{
    if (htk) {
        z <- 2595 * log10(1 + f/700)
    }
    else {
        f_0 <- 0
        f_sp <- 200/3
        brkfrq <- 1000
        brkpt <- (brkfrq - f_0)/f_sp
        logstep <- exp(log(6.4)/27)
        linpts <- (f < brkfrq)
        z <- 0 * f
        z[linpts] <- (f[linpts] - f_0)/f_sp
        z[!linpts] <- brkpt + (log(f[!linpts]/brkfrq))/log(logstep)
    }
    return(z)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
