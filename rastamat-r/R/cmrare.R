cmrare <- function(x, v1=c(1,2), v2=c(1,8), p=3, qmax=512, ...){
  if(is(x, "Wave")){
    x  <- spectempevo(x, ...)
  }

  dimx <- dim(x)

  if(!(is(x, "array") && length(dimx) == 3))
    stop("'x' has to be of class 'Wave' or a 3-dimensional array")

  xs <- sapply(seq(dimx[1]), function(y) apply(x[y,,], 1, sum))

  qq <- qr(outer(seq(qmax), 0:p, "^"))

  cmrare.fit <- function(n1, n2){
    y <- apply(xs[seq(qmax),(n1:n2)+1, drop=FALSE], 1, sum)/( (n2-n1+1) * xs[seq(qmax),1])
    return(qr.coef(qq, y))
  }

  b <- mapply(cmrare.fit, v1, v2)
  rownames(b) <- paste("p", 0:p, sep="")
  colnames(b) <- mapply(function(x,y) paste("v", "_", x, "_", y, sep=""), v1, v2)

  return(b)
}

# require(tuneR)
# a <- readWave("~/documents/Uni/Daten/McGill/piano-pl/pip_g4.wav")
# 
# #for(i in list.files("~/documents/Uni/Software/rastamat-r/R")) source(paste("~/documents/Uni/Software/rastamat-r/R/", i, sep=""))
# setwd("~/documents/Uni/Software/tuner/rastamat-r/R")
# source("powspec.R")
# source("specgram.R")
# source("windowfunctions.R")
# source("spectempevo.R")
# 
# print(system.time(stevos2 <- spectempevo(a)))
# print(gc())
# save(stevos2, file="~/stevos2.Rdata")
# 
# 
#Olaf: d <- qr(A); c1 <- qr.coef(d, y1); c2 <- qr.coef(d, y2) sollte dann die schnellste möglichtkeit sein.
