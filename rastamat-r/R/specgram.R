## Copyright (C) 1999-2000 Paul Kienzle
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; If not, see <http://www.gnu.org/licenses/>.

## FFTW support and input checks added by Sebastian Krey

specgram <- function(x, n = min(256, length(x)), Fs = 2, window =
    hanning.window(n), overlap = ceiling(length(window)/2)){
  
    if((!is.numeric(x)) || (!is.null(dim(x))))
      stop("'x' has to be a numeric vector.")

    lx <- length(x)
    lw <- length(window)
    # Create hanning window
    if (lw == 1) 
        window <- hanning.window(window)

    if (length(n) > 1) 
        stop("specgram does not handle frequency vectors yet")

    # Compute offsets
    win_size <- lw
    if (win_size > n) {
        n <- win_size
        warning("specgram fft size adjusted to", n)
    }
    step <- win_size - overlap

    # Construct matrix of data slices
    if (lx > win_size) 
        offset <- seq(1, lx-win_size, by=step)
    else offset <- 1
    lo <- length(offset)
    # xind <- unlist(lapply(offset, function(x) seq(x, x+win_size-1)))
    # x <- matrix(x[xind], nrow=lo, byrow=TRUE)
    # window <- matrix(rep(window, each=lo), nrow=lo)
    S <- matrix(0, n, lo)
    # S[1:win_size,] <- t(x * window)
    for (i in 1:lo) {
        S[1:win_size, i] = x[offset[i]:(offset[i]+win_size-1)] * window
    }

    # ps <- planFFT(n)
    # S <- apply(S, 2, function(x) FFT(x, plan=ps))
    S <- apply(S, 2, function(x) fft(x))

    # Return only positive frequency components
    if(n%%2 == 1){
        ret_n <- (n + 1)/2
    } else {
        ret_n <- n/2
    }
    S <- S[1:ret_n,]

    f <- (0:(ret_n-1)) * Fs/n
    t <- offset/Fs

    res <- list(S=S, f=f, t=t)
    class(res) <- "specgram"
    return(res)
}

print.specgram <- plot.specgram <- function(x, ...) {
  image(20*log10(t(abs(x$S))), col = gray(0:512 / 512), axes = FALSE, ...)
}

