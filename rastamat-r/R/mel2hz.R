mel2hz <- function(z, htk=FALSE){
    if(htk){
        f <- 700 * (10^(z/2595) - 1)
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

        linpts <- (z < brkpt)

        f <- 0 * z
        # Calculate f separately for linear- and log-spaced frequencies
        f[linpts] = f_0 + f_sp * z[linpts]
        f[!linpts] <- brkfrq * exp( log(logstep) * (z[!linpts] - brkpt) )
    }
    return(f)
}

