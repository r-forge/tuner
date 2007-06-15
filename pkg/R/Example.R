# Examples:
if(FALSE){
    library(tuneR)    
   
    dir <- "d:/uwe/R/library-devel/tuneR/R"
    for(i in file.path(dir, dir(dir))) try(source(i))

#################################################################################################

    TZ <- readWave("d:/TZHasse.wav")

    TZr <- mono(TZ, "right")
    TZr11 <- downsample(TZr, 11025)
    TZr11p3 <- extractWave(TZr11, 463700, 662300)    

    #rm(TZ, TZr, TZr11)    
    
    WspecObject <- periodogram(TZr11p3, normalize = TRUE, width = 512, overlap=256)

### Ab hier ist alles noch nicht ausgereift und wird mit sicherheit noch geaendert!!!!!

    notes <- noteFromFF(FF(WspecObject), 443.5)
    snotes <- smoother(notes)

    melodyplot(WspecObject, snotes, bars = 8)

    qnotes <- quantize(snotes, WspecObject@energy, parts = 8*8)
    quantplot(qnotes, bars = 8)

    q2notes <- quantMerge(snotes, minlength = 8, barsize = 8, bars = 8)
    lilyinput(q2notes, file = "d:/test.ly", time = "4/4", Major = TRUE, 
        key = "f", clef = "treble", tempo = "2 = 65")
  
# nur auf Uwe's Rechner zum Noten machen:

    system("c:/cygwin/bin/bash --login -c \"ly2dvi -P -p -o /cygdrive/d/test /cygdrive/d/test\" ")
    system("d:/Programme/gstools/gsview/gsview32.exe d:/test.ps", wait = FALSE)
}
