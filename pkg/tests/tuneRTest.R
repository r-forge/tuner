library("tuneR")
library("tools")

files <- tempfile(as.character(1:100), fileext = ".wav")

writeWave(sine(440, pcm=TRUE, bit=16), file = files[1])
writeWave(sine(440, pcm=TRUE, bit=8), file = files[2])

as.vector(md5sum(files[1:2]))
