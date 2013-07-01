library("tuneR")
(x <- readWave("Testfiles/8bit_PCM_mono.wav"))
x@left[1:10]
