hz2mel <- function(f, htk=FALSE){
    if(htk){
        z <- 2595 * log10(1 + f/700)
    } else {
        # Mel calculation like in Slaney's Auditory Toolbox
        f_0 <- 0
        f_sp <- 200/3
        brkfrq <- 1000
        # Starting Mel value for log region
        brkpt <- (brkfrq - f_0)/f_sp
        # The magic 1.0711703 which is the ratio needed to get from 1000 Hz to
        # 6400 Hz in 27 steps.
        logstep <- exp( log(6.4)/27 )

        linpts <- (f < brkfrq)

        z <- 0 * f

        z[linpts] <- (f[linpts] - f_0)/f_sp
        z[!linpts] <- brkpt + (log(f[!linpts]/brkfrq))/log(logstep)
    }
    return(z)
}

