library("tuneR")
library("tools")

## writeWave
files <- tempfile(as.character(1:100), fileext = ".wav")
## 8bit
writeWave(sine(440, pcm=TRUE, bit=8), file = files[1]) 
writeWave(stereo(sine(440, bit=8, pcm=TRUE), sine(220, bit=8, pcm=TRUE)), file = files[2])
## 16bit
writeWave(sine(440, pcm=TRUE, bit=16), file = files[3])
writeWave(stereo(sine(440, bit=16, pcm=TRUE), sine(220, bit=16, pcm=TRUE)), file = files[4])
## 24bit
writeWave(sine(440, pcm=TRUE, bit=24), file = files[5])
writeWave(stereo(sine(440, bit=24, pcm=TRUE), sine(220, bit=24, pcm=TRUE)), file = files[6])
## 32bit
writeWave(sine(440, bit=32), file = files[7])
writeWave(sine(440, pcm=TRUE, bit=32), file = files[8])
writeWave(stereo(sine(440, bit=32), sine(220, bit=32)), file = files[9])
writeWave(stereo(sine(440, bit=32, pcm=TRUE), sine(220, bit=32, pcm=TRUE)), file = files[10])
## 64bit
writeWave(sine(440, bit=64), file = files[11])
writeWave(stereo(sine(440, bit=64), sine(220, bit=64)), file = files[12])
as.vector(md5sum(files[1:12]))

## Waveforms
## sine
(x1 <- sine(440, pcm=TRUE, bit=8))
length(x1@left)
x1@stereo
x1@samp.rate
x1@bit
x1@pcm
(x2 <- stereo(sine(440, bit=8, pcm=TRUE), sine(220, bit=8, pcm=TRUE)))
length(x2@left)
x2@stereo
x2@samp.rate
x2@bit
x2@pcm
(x3 <- sine(440, bit=32, duration = 1000))
length(x3@left)
x3@stereo
x3@samp.rate
x3@bit
x3@pcm
(x4 <- sine(440, bit=8, pcm=TRUE, samp.rate=11025))
length(x4@left)
x4@stereo
x4@samp.rate
x4@bit
x4@pcm 
(x5 <- sine(440, bit=8, pcm=TRUE, stereo = TRUE))
length(x5@left)
x5@stereo
x5@samp.rate
x5@bit
x5@pcm

## normalize
obj <- sine(440, pcm=TRUE, bit=8)
obj2 <- stereo(sine(440, bit=8, pcm=TRUE), sine(220, bit=8, pcm=TRUE))
(x6 <- normalize(obj, unit = "1", center = TRUE, level = 1, rescale = TRUE))
x6@left[1:10]
(x7 <- normalize(obj, unit = "1", center = TRUE, level = 1, rescale = FALSE))
x7@left[1:10]
(x8 <- normalize(obj, unit = "8", center = TRUE, level = 2, rescale = TRUE))
x8@left[1:10]
(x9 <- normalize(obj, unit = "8", center = TRUE, level = 2, rescale = FALSE))
x9@left[1:10]
(x10 <- normalize(obj, unit = "16", center = TRUE, level = 1, rescale = TRUE)) 
x10@left[1:10]
(x12 <- normalize(obj, unit = "16", center = TRUE, level = 1, rescale = FALSE)) 
x12@left[1:10]
(x13 <- normalize(obj, unit = "24", center = TRUE, level = 1, rescale = TRUE))
13@left[1:10]
(x14 <- normalize(obj, unit = "24", center = TRUE, level = 1, rescale = FALSE))
14@left[1:10]
(x15 <- normalize(obj, unit = "32", center = TRUE, level = 1, rescale = TRUE))
x15@left[1:10]
(x16 <- normalize(obj, unit = "32", center = TRUE, level = 1, rescale = FALSE))
x16@left[1:10]
(x17 <- normalize(obj, unit = "32", center = TRUE, level = 1, rescale = TRUE, pcm = FALSE))
x17@left[1:10]
(x18 <- normalize(obj, unit = "32", center = TRUE, level = 1, rescale = FALSE, pcm = FALSE))
x18@left[1:10]
(x19 <- normalize(obj, unit = "64", center = TRUE, level = 1, rescale = TRUE, pcm = FALSE))
x19@left[1:10]
(x20 <- normalize(obj, unit = "64", center = TRUE, level = 1, rescale = FALSE, pcm = FALSE))
x20@left[1:10]
(x21 <- normalize(obj2, unit = "1", center = TRUE, level = 1, rescale = TRUE))
x21@left[1:10]
x21@right[1:10]
(x22 <- normalize(obj2, unit = "1", center = TRUE, level = 1, rescale = FALSE))
x22@left[1:10]
(x23 <- normalize(obj2, unit = "8", center = TRUE, level = 2, rescale = TRUE))
x23@left[1:10]
(x24 <- normalize(obj2, unit = "8", center = TRUE, level = 2, rescale = FALSE))
x24@left[1:10]
(x25 <- normalize(obj2, unit = "16", center = TRUE, level = 1, rescale = TRUE)) 
x25@left[1:10]
(x26 <- normalize(obj2, unit = "16", center = TRUE, level = 1, rescale = FALSE)) 
x26@left[1:10]
(x27 <- normalize(obj2, unit = "24", center = TRUE, level = 1, rescale = TRUE))
x27@left[1:10]
(x28 <- normalize(obj2, unit = "24", center = TRUE, level = 1, rescale = FALSE))
x28@left[1:10]
(x29 <- normalize(obj2, unit = "32", center = TRUE, level = 1, rescale = TRUE))
x29@left[1:10]
(x30 <- normalize(obj2, unit = "32", center = TRUE, level = 1, rescale = FALSE))
x30@left[1:10]
(x31 <- normalize(obj2, unit = "32", center = TRUE, level = 1, rescale = TRUE, pcm = FALSE))
x31@left[1:10]
(x32 <- normalize(obj2, unit = "32", center = TRUE, level = 1, rescale = FALSE, pcm = FALSE))
x32@left[1:10]
(x33 <- normalize(obj2, unit = "64", center = TRUE, level = 1, rescale = TRUE, pcm = FALSE))
x33@left[1:10]
(x34 <- normalize(obj2, unit = "64", center = TRUE, level = 1, rescale = FALSE, pcm = FALSE))
x34@left[1:10]