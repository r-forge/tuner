\name{specgram}
\alias{specgram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
## usage: [S [, f [, t]]] = specgram(x [, n [, Fs [, window [, overlap]]]])
##
## Generate a spectrogram for the signal. This chops the signal into
## overlapping slices, windows each slice and applies a Fourier
## transform to determine the frequency components at that slice.
##
## x: vector of samples
## n: size of fourier transform window, or [] for default=256
## Fs: sample rate, or [] for default=2 Hz
## window: shape of the fourier transform window, or [] for default=hanning(n)
##    Note: window length can be specified instead, in which case
##    window=hanning(length)
## overlap: overlap with previous window, or [] for default=length(window)/2
##
## Return values
##    S is complex output of the FFT, one row per slice
##    f is the frequency indices corresponding to the rows of S.
##    t is the time indices corresponding to the columns of S.
##    If no return value is requested, the spectrogram is displayed instead.
##
## Example
##    x = chirp([0:0.001:2],0,2,500);  # freq. sweep from 0-500 over 2 sec.
##    Fs=1000;                  # sampled every 0.001 sec so rate is 1 kHz
##    step=ceil(20*Fs/1000);    # one spectral slice every 20 ms
##    window=ceil(100*Fs/1000); # 100 ms data window
##    specgram(x, 2^nextpow2(window), Fs, window, window-step);
##
##    ## Speech spectrogram
##    [x, Fs] = auload(file_in_loadpath("sample.wav")); # audio file
##    step = fix(5*Fs/1000);     # one spectral slice every 5 ms
##    window = fix(40*Fs/1000);  # 40 ms data window
##    fftn = 2^nextpow2(window); # next highest power of 2
##    [S, f, t] = specgram(x, fftn, Fs, window, window-step);
##    S = abs(S(2:fftn*4000/Fs,:)); # magnitude in range 0<f<=4000 Hz.
##    S = S/max(S(:));           # normalize magnitude so that max is 0 dB.
##    S = max(S, 10^(-40/10));   # clip below -40 dB.
##    S = min(S, 10^(-3/10));    # clip above -3 dB.
##    imagesc(t, f, flipud(log(S)));   # display in log scale
##
## The choice of window defines the time-frequency resolution.  In
## speech for example, a wide window shows more harmonic detail while a
## narrow window averages over the harmonic detail and shows more
## formant structure. The shape of the window is not so critical so long
## as it goes gradually to zero on the ends.
##
## Step size (which is window length minus overlap) controls the
## horizontal scale of the spectrogram. Decrease it to stretch, or
## increase it to compress. Increasing step size will reduce time
## resolution, but decreasing it will not improve it much beyond the
## limits imposed by the window size (you do gain a little bit,
## depending on the shape of your window, as the peak of the window
## slides over peaks in the signal energy).  The range 1-5 msec is good
## for speech.
##
## FFT length controls the vertical scale.  Selecting an FFT length
## greater than the window length does not add any information to the
## spectrum, but it is a good way to interpolate between frequency
## points which can make for prettier spectrograms.
##
## After you have generated the spectral slices, there are a number of
## decisions for displaying them.  First the phase information is
## discarded and the energy normalized:
##
##     S = abs(S); S = S/max(S(:));
##
## Then the dynamic range of the signal is chosen.  Since information in
## speech is well above the noise floor, it makes sense to eliminate any
## dynamic range at the bottom end.  This is done by taking the max of
## the magnitude and some minimum energy such as minE=-40dB. Similarly,
## there is not much information in the very top of the range, so
## clipping to a maximum energy such as maxE=-3dB makes sense:
##
##     S = max(S, 10^(minE/10)); S = min(S, 10^(maxE/10));
##
## The frequency range of the FFT is from 0 to the Nyquist frequency of
## one half the sampling rate.  If the signal of interest is band
## limited, you do not need to display the entire frequency range. In
## speech for example, most of the signal is below 4 kHz, so there is no
## reason to display up to the Nyquist frequency of 10 kHz for a 20 kHz
## sampling rate.  In this case you will want to keep only the first 40%
## of the rows of the returned S and f.  More generally, to display the
## frequency range [minF, maxF], you could use the following row index:
##
##     idx = (f >= minF & f <= maxF);
##
## Then there is the choice of colormap.  A brightness varying colormap
## such as copper or bone gives good shape to the ridges and valleys. A
## hue varying colormap such as jet or hsv gives an indication of the
## steepness of the slopes.  The final spectrogram is displayed in log
## energy scale and by convention has low frequencies on the bottom of
## the image:
##
##     imagesc(t, f, flipud(log(S(idx,:))));

## 2001-07-05 Paul Kienzle <pkienzle@users.sf.net>
## * remove "See also spectrogram"
## * add notes on selecting parameters for the spectrogram
}
\usage{
specgram(x, n = min(256, length(x)), Fs = 2, window = hanning.window(n), overlap = ceiling(length(window)/2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{Fs}{ ~~Describe \code{Fs} here~~ }
  \item{window}{ ~~Describe \code{window} here~~ }
  \item{overlap}{ ~~Describe \code{overlap} here~~ }
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
