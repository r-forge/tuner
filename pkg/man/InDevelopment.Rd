\name{tuneRDevel}
\alias{dynquant} 
\alias{metrum} 
\alias{plotDD} 
\alias{tempo}

\title{tuneR function in development}
\description{These functions are still in development and under heavy reconstructering, hence mainly undocumented. 
    See Details for some information on their purpose.
    Not all mentioned functions are yet published in the CRAN version of this package.}
\usage{
dynquant(objectSinger, notes, objectPiano, upper = 0.75, 
    lvquart = 9, f = 0.002, delta = 3, bars = 26.25, 
    energyplot = FALSE, minln = lquart / 2, ...)

metrum(energy, f = 0.005, delta = ceiling(lquart), lvquart = 9, 
    bars = 26.25, upper = 0.75, energyplot = FALSE, ...)

plotDD(object, tone, ...)

tempo(energy, upper = 0.75, lvquart = 9, f = 0.002, delta = 3, 
    bars = 26.25, energyplot = FALSE, minln = lquart / 2, ...)
}

\arguments{
    Arguments are not yet documented.
}
\details{\describe{
    \item{\code{dynquant}}{One of the first (bad) approaches for dynamic quantization.}
    \item{\code{metrum}}{Deriving metrum such as 3/4 or 4/4 (which are the only supported ones yet) 
        from an energy vector.}
    \item{\code{plotDD}}{Plots detected melody (notes) and energy vs. time 
        - already devided into bars by qunatization. 
        Can also plot expected values into the same plot. See \code{melodyplot}.}
    \item{\code{tempo}}{Function to derive tempo from an energy vector.}
}}
\note{These functions are heavily in development. Names, underlying algorithms, arguemnts etc. may change!}
\value{
    Values are not yet documented.
}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\seealso{\code{\link{Wave}}, \code{\link{Wspec}}, \code{\link{periodogram}}, \code{\link{FF}}}
\keyword{internal}
\keyword{misc}
\keyword{hplot}
\keyword{ts}

