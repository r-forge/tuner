deltas <- function(x, w=9){
    nr <- nrow(x)
    nc <- ncol(x)

    # Define windows shape
    hlen <- floor(w/2)
    w <- 2 * hlen + 1
    win <- seq(hlen, -hlen, -1)

    # Pad data
    xx <- matrix(c(rep(x[,1], hlen), x, rep(x[,1], hlen)), nrow=nr)

    # Delta filtering alog rows
    d <-  t(apply(xx, 1, function(x) {convolve(x, win, conj=FALSE, type="open")[-(1:(length(win)-1))]}))

    # Trim edges
    d <- d[,2 * hlen + 1:nc]

    return(d)
}

