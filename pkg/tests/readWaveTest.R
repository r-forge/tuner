library("tuneR")
(x1 <- readWave("Testfiles/8bit_PCM_mono.wav"))
x1@left[1:10]
(x2 <- readWave("Testfiles/8bit_PCM_stereo.wav"))
x2@left[1:10]
x2@right[1:10]
(x3 <- readWave("Testfiles/16bit_PCM_mono.wav"))
x3@left[1:10]
(x4 <- readWave("Testfiles/16bit_PCM_stereo.wav"))
x4@left[1:10]
x4@right[1:10]
(x5 <- readWave("Testfiles/24bit_PCM_mono.wav"))
x5@left[1:10]
(x6 <- readWave("Testfiles/24bit_PCM_mono_ex.wav"))
x6@left[1:10]
(x7 <- readWave("Testfiles/24bit_PCM_stereo.wav"))
x7@left[1:10]
x7@right[1:10]
(x8 <- readWave("Testfiles/24bit_PCM_stereo_ex.wav"))
x8@left[1:10]
x8@right[1:10]
(x9 <- readWave("Testfiles/32bit_float_mono.wav"))
x9@left[1:10]
(x10 <- readWave("Testfiles/32bit_float_stereo.wav"))
x10@left[1:10]
x10@right[1:10]
(x12 <- readWave("Testfiles/32bit_PCM_mono.wav"))
x12@left[1:10]
(x13 <- readWave("Testfiles/32bit_PCM_mono_ex.wav"))
x13@left[1:10]
(x14 <- readWave("Testfiles/32bit_PCM_stereo.wav"))
x14@left[1:10]
x14@right[1:10]
(x15 <- readWave("Testfiles/32bit_PCM_stereo_ex.wav"))
x15@left[1:10]
x15@right[1:10]
(x16 <- readWave("Testfiles/64bit_float_mono.wav"))
x16@left[1:10]
(x17 <- readWave("Testfiles/64bit_float_stereo.wav"))
x17@left[1:10]
x17@right[1:10]
