# In Development
ispecgram <- function(d, ftsize = -1, sr = -1, win = -1, nov = -1){
  if (ftsize == -1) {
    ftsize <- 2*(nrow(d)-1)
  }
  if (win == -1) {
    win <- ftsize
  }
  if (nov == -1) {
    nov <- ftsize/2
  }

  # return(x)
}
