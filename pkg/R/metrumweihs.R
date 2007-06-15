tempo <- function(energy, upper = 0.75, lvquart = 9, f = 0.002, delta = 3, bars = 26.25, 
    energyplot = FALSE, minln = lquart / 2, ...){

    # f, delta: lowess Parameter
    # lvquart: Laenge des Vorspiels in Vierteln
    # bars: Anzahl Takte
    # upper: threshold

    require(pastecs)
    lquart <- length(energy) / (4 * bars) # statische Quantisierung Viertel
    vorspiel <- lquart * lvquart # Laenge des Vorspiels in Beobachtungen
    # tempo identification
    energylst <- lowess(energy, f = f, delta = delta)
    if(energyplot){
        plot (energy, type = "l", ...)
        lines (energylst, col = "red", ...)
    }
    turnplst <- which(turnpoints(energylst$y)$peaks)
    maximalst <- turnplst[energylst$y[turnplst] > upper] # Stellen, an denen Wendepunkte mit Werten > upper auftreten
    difflst <- diff(maximalst) / (minln / 2)
    index <- which(round(difflst/2) == 2)
    tempo <- c(round(lquart)    , difflst[index] * (minln / 2))
    # takt <- round ((maximalst[which(round(difflst/2) == 2)]-vorspiel) / (4*lquart)) + 1
    maximalst <- c(1, maximalst[index])
    return(list(maximalst=maximalst, tempo=tempo))
}


metrum <- function(energy, f = 0.005, delta = ceiling(lquart), lvquart = 9, 
    bars = 26.25, upper = 0.75, energyplot = FALSE, ...){

    require(pastecs)
    lquart <- length(energy) / (4 * bars) # statische Quantisierung Viertel    
    energylsm <- lowess(energy, f = f, delta = delta)
    if(energyplot){
        plot(energy, type = "l", ...)
        lines(energylsm, col = "red", lwd = 2, ...)
        abline(h = upper, col = "pink", ...)
    }
    turnplsm <- which(turnpoints(energylsm$y)$peaks)
    maximalsm <- round((turnplsm[energylsm$y[turnplsm] > upper] / lquart) - lvquart + 1)
    difflsm <- diff(maximalsm)
    # cat(difflsm, "\n")
    anz44 <- sum(!(difflsm %% 4))
    anz34 <- sum(!(difflsm %% 3))
    return(list(anz34=anz34, anz44=anz44))
}


dynquant <- function(objectSinger, notes, objectPiano, upper = 0.75, lvquart = 9, f = 0.002, delta = 3, bars = 26.25, 
    energyplot = FALSE, minln = lquart / 2, ...){

    energy <- objectPiano@energy
    energy <- energy / max(energy)
    lquart <- length(energy) / (4 * bars) # statische Quantisierung Viertel    
    
    me <- metrum(energy, upper = upper, lvquart = lvquart, f = f, delta = delta, bars = bars, 
        energyplot = energyplot, ...)
    four <- me$anz34 <= me$anz44
    te <- tempo(energy, upper = upper, lvquart = lvquart, f = f, delta = delta, bars = bars, 
        energyplot = energyplot, minln = minln, ...)
    
    tempo <- te$tempo
    maximalst <- te$maximalst
    vorspiellaenge <- tempo[1] * lvquart
    if(vorspiellaenge){
        index <- maximalst > vorspiellaenge
        index[which(index)[1]] <- TRUE
        maximalst <- maximalst[index] - vorspiellaenge
        maximalst[1] <- 1
        tempo <- tempo[index]
        index <- (round(vorspiellaenge) + 1):length(objectSinger@spec)
        objectSinger <- objectSinger[index]
        notes <- notes[index]
    }
    n <- length(objectSinger@spec)
    Energie <- objectSinger@energy
    
    starts <- 1
    for(i in seq(along = maximalst[-1]))
        starts <- c(starts, seq(starts[length(starts)], maximalst[i+1], by = tempo[i] / 2))
    starts <- c(starts, seq(starts[length(starts)], n, by = tempo[length(tempo)] / 2))
    starts <- unique(round(starts))
    ends <- (starts-1)[-1]
    nnotes <- length(ends)
    ends <- c(ends, n)
    starts <- starts
    notes <- mapply(function(x, y) notes[x:y], x=starts, y=ends)
    newnotes <- sapply(notes, function(x){x <- table(x); names(x[which.max(x)])})
    newnotes[which(!sapply(newnotes, length))] <- NA
    Energie <- mapply(function(x, y) Energie[x:y], x=starts, y=ends)
    Energie <- sapply(Energie, mean)
    return(list(notes = round(as.numeric(unlist(newnotes))), energy = Energie))
}
