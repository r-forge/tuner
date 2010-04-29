plotDD <-
function(object, tone, ...){
  index <- (nrow(object) - (832:55))[-(350:358)]
  expNote <- object[index, 1]
  isNote <-  object[index, 2]
  isNote[isNote == -100] <- NA
  rg <- range(isNote, expNote, na.rm = TRUE)
  y.ticks <- c("Silence", tone$name[which(tone$no %in% rg[1]:rg[2])])
  rg.s <- rg[1] - 2
  isNote[is.na(isNote)] <- rg.s
  x <- seq(along = expNote) / 96
  plot(x, expNote - .05, main = i, type = "l", lwd = 2, xaxs = "i",
    xlab = "Bar", ylab = "Note", xaxt = "n", yaxt = "n", 
    ylim = c(rg.s, rg[2]))
  axis(1, at = 1:8)
  axis(2, at = (rg.s:rg[2])[-2], label = as.character(y.ticks))
  lines(x, isNote + .05, col = "red", lwd = 2)
  abline(v = 1:7, col = "grey")
  abline(h = rg[1]:(rg[2]-1) + 0.5, col = "grey")
  abline(h = rg.s + 1.5)
  box()
}
