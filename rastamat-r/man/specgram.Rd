\name{specgram}
\alias{specgram}
\title{ Spectogram }
\description{
Generate a spectrogram for the signal. This chops the signal into
overlapping slices, windows each slice and applies a Fourier
transform to determine the frequency components at that slice.
}
\usage{
specgram(x, n = min(256, length(x)), Fs = 2, window = hanning.window(n), overlap = ceiling(length(window)/2))
}
\arguments{
  \item{x}{ the vector of samples. }
  \item{n}{ the size of the Fourier transform window. }
  \item{Fs}{ the sample rate, Hz. }
  \item{window}{ shape of the fourier transform window, defaults to
    \code{hanning(n)}. The window length for a hanning window can be
    specified instead. }
  \item{overlap}{ overlap with previous window, defaults to half the
    window length. }
  \item{\ldots}{ additional arguments (ignored). }
}
\details{
The choice of window defines the time-frequency resolution.  In
speech for example, a wide window shows more harmonic detail while a
narrow window averages over the harmonic detail and shows more
formant structure. The shape of the window is not so critical so long
as it goes gradually to zero on the ends.

Step size (which is window length minus overlap) controls the
horizontal scale of the spectrogram. Decrease it to stretch, or
increase it to compress. Increasing step size will reduce time
resolution, but decreasing it will not improve it much beyond the
limits imposed by the window size (you do gain a little bit,
depending on the shape of your window, as the peak of the window
slides over peaks in the signal energy).  The range 1-5 msec is good
for speech.

FFT length controls the vertical scale.  Selecting an FFT length
greater than the window length does not add any information to the
spectrum, but it is a good way to interpolate between frequency
points which can make for prettier spectrograms.

After you have generated the spectral slices, there are a number of
decisions for displaying them.  First the phase information is
discarded and the energy normalized:

    \code{S = abs(S); S = S/max(S)}

Then the dynamic range of the signal is chosen.  Since information in
speech is well above the noise floor, it makes sense to eliminate any
dynamic range at the bottom end.  This is done by taking the max of
the magnitude and some minimum energy such as minE=-40dB. Similarly,
there is not much information in the very top of the range, so
clipping to a maximum energy such as maxE=-3dB makes sense:

   \code{S = max(S, 10^(minE/10)); S = min(S, 10^(maxE/10))}

The frequency range of the FFT is from 0 to the Nyquist frequency of
one half the sampling rate.  If the signal of interest is band
limited, you do not need to display the entire frequency range. In
speech for example, most of the signal is below 4 kHz, so there is no
reason to display up to the Nyquist frequency of 10 kHz for a 20 kHz
sampling rate.  In this case you will want to keep only the first 40\%
of the rows of the returned \code{S} and \code{f}.  More generally, to display the
frequency range \code{[minF, maxF]}, you could use the following row index:

    \code{idx = (f >= minF & f <= maxF)}

Then there is the choice of colormap.  A brightness varying colormap
such as copper or bone gives good shape to the ridges and valleys. A
hue varying colormap such as jet or hsv gives an indication of the
steepness of the slopes.  The final spectrogram is displayed in log
energy scale and by convention has low frequencies on the bottom of
the image.

}
\value{
  \item{S }{complex output of the FFT, one row per slice. }
  \item{f }{the frequency indices corresponding to the rows of S.}
  \item{t }{the time indices corresponding to the columns of S..}
}
\references{
  Octave Forge \url{http://octave.sf.net}
}
\author{ Original Octave version by Paul Kienzle
  \email{pkienzle@users.sf.net}. Conversion to R by Tom Short. }
\seealso{ \code{\link{fft}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, n = min(256, length(x)), Fs = 2, window = hanning.window(n), 
    overlap = ceiling(length(window)/2)) 
{
    lx <- length(x)
    lw <- length(window)
    if (lw == 1) 
        window <- hanning.window(window)
    if (length(n) > 1) 
        stop("specgram does not handle frequency vectors yet")
    win_size <- lw
    if (win_size > n) {
        n <- win_size
        warning("specgram fft size adjusted to", n)
    }
    step <- win_size - overlap
    if (lx > win_size) 
        offset <- seq(1, lx - win_size, by = step)
    else offset <- 1
    lo <- length(offset)
    S <- matrix(0, n, lo)
    for (i in 1:lo) {
        S[1:win_size, i] = x[offset[i]:(offset[i] + win_size - 
            1)] * window
    }
    ps <- planFFT(n)
    S <- apply(S, 2, function(x) FFT(x, plan = ps))
    if (n\%\%2 == 1) {
        ret_n <- (n + 1)/2
    }
    else {
        ret_n <- n/2
    }
    S <- S[1:ret_n, ]
    f <- (0:(ret_n - 1)) * Fs/n
    t <- offset/Fs
    return(list(S = S, f = f, t = t))
  }
}
\keyword{ spectrogram }
\keyword{ spectrum }
