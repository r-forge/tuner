writeWave <- 
function(object, filename){
    if(!is(object, "Wave")) 
        stop("'object' needs to be of class 'Wave'")
    validObject(object)

    if(object@stereo){
        sample.data <- matrix(c(object@left, object@right), nrow = 2, byrow = TRUE)
        dim(sample.data) <- NULL
    }
    else sample.data <- object@left
    
    ## PCM or IEEE FLOAT
    pcm <- object@pcm                                 
    
    if(pcm) {                                                                                     
      if((object@bit == 8) && ( (max(sample.data) > 255) || (min(sample.data) < 0) ))              
          stop("for 8-bit Wave files, data range is supposed to be in [0, 255], see ?normalize")
      if((object@bit == 16) && ( (max(sample.data) > 32767) || (min(sample.data) < -32768)))
          stop("for 16-bit Wave files, data range is supposed to be in [-32768, 32767], see ?normalize")
      if((object@bit == 24) && ( (max(sample.data) > 8388607) || (min(sample.data) < -8388608)))
          stop("for 24-bit Wave files, data range is supposed to be in [-8388608, 8388607], see ?normalize")
      if((object@bit == 32) && ( (max(sample.data) > 2147483647) || (min(sample.data) < -2147483648)))
          stop("for 32-bit Wave files, data range is supposed to be in [-2147483648, 2147483647], see ?normalize")
      if(any(sample.data %% 1 > 0))
          warning("channels' data will be rounded to integers for writing the wave file")
    } else {                                                                                    
      if( (max(sample.data) > 1) || (min(sample.data) < -1) )
          stop("for IEEE float Wave files, data range is supposed to be in [-1,1]") 
    }
    
    # Open connection
    con <- file(filename, "wb")
    on.exit(close(con)) # be careful ...
        
    # Some calculations:
    l <- length(object@left)
    byte <- as.integer(object@bit / 8)
    channels <- object@stereo + 1
    block.align <- channels * byte
    bytes <- l * byte * channels
            
    ## Writing the header:
    # RIFF
    writeChar("RIFF", con, 4, eos = NULL)
    writeBin(as.integer(bytes + 36), con, size = 4, endian = "little")
    # WAVE
    writeChar("WAVE", con, 4, eos = NULL)
    # fmt chunk
    writeChar("fmt ", con, 4, eos = NULL)
    writeBin(as.integer(16), con, size = 4, endian = "little")
    ## Fallunterscheidung                                                       
    if(pcm) {    
      writeBin(as.integer(1), con, size = 2, endian = "little")
    } else {
      writeBin(as.integer(3), con, size = 2, endian = "little")
    }
      
    writeBin(as.integer(channels), con, size = 2, endian = "little")
    writeBin(as.integer(object@samp.rate), con, size = 4, endian = "little")
    writeBin(as.integer(object@samp.rate * block.align), con, size = 4, endian = "little")
    writeBin(as.integer(block.align), con, size = 2, endian = "little")
    writeBin(as.integer(object@bit), con, size = 2, endian = "little")
    #if(extensible) {
    #  writeBin(as.integer(22), con, size = 4, endian = "little")
    #  #wValidBitsPerSample  (evtl. abweichend von bitspersample?)
    #  writeBin(as.integer(object@bit), con, size = 2, endian = "little")
    #  #dwChannelMask (hier muss angegeben werden, wie die Zuordnung zu den einzelnen Channels erfolgt, auch übergeben?)
    #  writeBin(as.integer(1), con, size = 2, endian = "little")
    #  #SubFormat      ## irgendwie kommt das in den Beispielen nicht danach, in was für einem Fomrat??
    #  writeBin(as.character("\x00\x00\x00\x00\x10\x00\x80\x00\x00\xAA\x00\x38\x9B\x71"), con, size = 14, endian = "little")
    #  if(waveformat = pcm)    
    #    writeBin(as.integer(1), con, size = 2, endian = "little")
    #  if(waveformat = ieee)    
    #    writeBin(as.integer(3), con, size = 2, endian = "little")
    #}
    
    # fact
    #if(extensible) {
    #}
    # data
    writeChar("data", con, 4, eos = NULL)
    writeBin(as.integer(bytes), con, size = 4, endian = "little")

    # Write data:
    # PCM format
    if(pcm) { 
      if(byte == 3){
          sample.data <- sample.data + 2^24 * (sample.data < 0)
          temp <- sample.data %% (256^2)
          sample.data <- sample.data %/% 256^2
          a2 <- temp %/% 256
          temp <- temp %%  256
          writeBin(as.integer(rbind(temp, a2, sample.data)), con, size = 1, endian = "little")
      } else {
          writeBin(as.integer(sample.data), con, size = byte, endian = "little")
      }
    } else {
      writeBin(as.numeric(sample.data), con, size = byte, endian = "little")
    }

    invisible(NULL)
}
