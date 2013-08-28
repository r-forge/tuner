## 8bit_PCM_mono
x1 <- sine(440, bit=8, pcm=TRUE, duration=2000)
writeWave(x1, filename = "8bit_PCM_mono_ex.wav")
## sox 8bit_PCM_mono_ex.wav -t wavpcm 8bit_PCM_mono.wav
## 8bit_PCM_stereo
x2 <- stereo(sine(440, bit=8, pcm=TRUE, duration=2000), sine(220, bit=8, pcm=TRUE, duration=2000)) 
writeWave(x2, filename = "8bit_PCM_stereo_ex.wav")
## sox 8bit_PCM_stereo_ex.wav -t wavpcm 8bit_PCM_stereo.wav

## 16bit_PCM_mono
x3 <- sine(440, bit=16, pcm=TRUE, duration=2000)
writeWave(x3, filename = "16bit_PCM_mono_ex.wav")
## sox 16bit_PCM_mono_ex.wav -t wavpcm 16bit_PCM_mono.wav
## 16bit_PCM_stereo
x4 <- stereo(sine(440, bit=16, pcm=TRUE, duration=2000), sine(220, bit=16, pcm=TRUE, duration=2000))
writeWave(x4, filename = "16bit_PCM_stereo_ex.wav") 
## sox 16bit_PCM_mono_ex.wav -t wavpcm 16bit_PCM_mono.wav

## 24bit_PCM_mono
x5 <- sine(440, bit=24, pcm=TRUE, duration=2000)
writeWave(x5, filename = "24bit_PCM_mono_ex.wav")
## sox 24bit_PCM_mono_ex.wav -t wavpcm 24bit_PCM_mono.wav
## 24bit_PCM_stereo
x6 <- stereo(sine(440, bit=24, pcm=TRUE, duration=2000), sine(220, bit=24, pcm=TRUE, duration=2000))
writeWave(x6, filename = "24bit_PCM_stereo_ex.wav") 
## sox 24bit_PCM_mono_ex.wav -t wavpcm 24bit_PCM_mono.wav

## 32bit_PCM_mono
x7 <- sine(440, bit=32, pcm=TRUE, duration=2000)
writeWave(x7, filename = "32bit_PCM_mono_ex.wav")
## sox 32bit_PCM_mono_ex.wav -t wavpcm 32bit_PCM_mono.wav
## 32bit_PCM_stereo
x8 <- stereo(sine(440, bit=32, pcm=TRUE, duration=2000), sine(220, bit=32, pcm=TRUE, duration=2000))
writeWave(x8, filename = "32bit_PCM_stereo_ex.wav")
## sox 32bit_PCM_mono_ex.wav -t wavpcm 32bit_PCM_mono.wav 

## 32bit_float_mono
x9 <- sine(440, bit=32, pcm=FALSE, duration=2000)
writeWave(x9, filename = "32bit_float_mono_ex.wav")
## 32bit_float_stereo
x10 <- stereo(sine(440, bit=32, pcm=FALSE, duration=2000), sine(220, bit=32, pcm=FALSE, duration=2000)) 
writeWave(x10, filename = "32bit_float_stereo_ex.wav")

## 64bit_float_mono
exact64 <- c(-2^(-40:0), 2^(-40:0))
x12 <- Wave(left=exact64, bit=64, pcm=FALSE, samp.rate=44100)
writeWave(x12, filename = "64bit_float_mono_ex.wav")
## 64bit_float_stereo
x13 <- stereo(Wave(left=exact64, bit=64, pcm=FALSE, samp.rate=44100), Wave(left=rev(exact64), bit=64, pcm=FALSE, samp.rate=44100))
writeWave(x13, filename = "64bit_float_stereo_ex.wav") 

## 16bit_PCM_3channels
x14 <- WaveMC(cbind(sine(660, bit=16, pcm=TRUE, duration=2000)@left,
                    sine(440, bit=16, pcm=TRUE, duration=2000)@left, 
                    sine(220, bit=16, pcm=TRUE, duration=2000)@left), bit=16, samp.rate=44100) 
## first three channels, sorted
colnames(x14@.Data) <- c("FL","FR","FC")
writeWave(x14, filename = "16bit_PCM_3channels_ex.wav") 

## 32bit_float_4channels
x15 <- WaveMC(cbind(sine(660, bit=32, pcm=FALSE, duration=2000)@left,
                    sine(440, bit=32, pcm=FALSE, duration=2000)@left, 
                    sine(440, bit=32, pcm=FALSE, duration=2000)@left,
                    sine(220, bit=32, pcm=FALSE, duration=2000)@left), bit=32, samp.rate=44100, pcm=FALSE) 
## channels omitted and not sorted
colnames(x15@.Data) <- c("FL","FR","BR","BL")
writeWave(x15, filename = "32bit_float_4channels_ex.wav") 
