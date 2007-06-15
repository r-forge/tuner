melodyplot <- 
function(object, observed, expected = NULL, bars = NULL, main = NULL, 
    xlab = NULL, ylab = "note", xlim = NULL, ylim = NULL, 
    observedcol = "red", expectedcol = "grey", gridcol = "grey",
    lwd = 2, las = 1, cex.axis = 0.9, mar = c(5, 4, 4, 4) + 0.1,
    notenames = NULL, silence = "silence", plotenergy = TRUE, ...,
    axispar = list(ax1 = list(side=1), ax2 = list(side=2), ax4 = list(side=4)),
    boxpar = list(), 
    energylabel = list(text="energy", side=4, line=2.5, at=rg.s-0.25, las=3),
    energypar = list(), 
    expectedpar = list(),
    gridpar = list(col = gridcol), 
    observedpar = list(col = observedcol, lwd = 2))
{

    opar <- par(las = las, cex.axis = cex.axis, mar = mar)
    on.exit(par(opar))

    if(is.null(bars)){
        starts <- object@starts
        bars <- starts[length(starts)] + object@width
        bars <- bars / object@samp.rate 
        if(is.null(xlab)) xlab <- "time"
    }
    else if(is.null(xlab)) xlab <- "bar"
    
    rg <- range(observed, expected, na.rm = TRUE)
    if(is.null(notenames)) notenames <- notenames(rg[1]:rg[2])
    y.ticks <- c(silence, notenames)
    rg.s <- rg[1] - 2
    observed[is.na(observed)] <- rg.s
    x <- bars * seq(0, 1, length = length(observed))
    if(is.null(xlim)) xlim <- c(0, if(bars < 1) 1 else bars)
    if(is.null(ylim)) ylim <- c(rg.s - 2 , rg[2] + 0.5)

    ## setup pf the plot itself:
    plot(x, observed - .05, xaxt = "n", yaxt = "n", 
        main = main, xlab = xlab, ylab = ylab, 
        type = "n", lwd = lwd, xaxs = "i", yaxs = "i",
        xlim = xlim, ylim = ylim, ...)
    do.call("axis", c(list(at = 1:bars), axispar[["ax1"]]))


    if(!is.null(expected)) 
        do.call("rect", 
            c(list(xleft = c(0, x[-length(x)]), ybottom = expected-.5, 
                   xright = x,                  ytop =    expected+.5),
                   border = expectedcol, col = expectedcol,
              expectedpar))
    do.call("axis", 
        c(list(at = (rg.s:rg[2])[-2], label = as.character(y.ticks)), 
          axispar[["ax2"]]))
    do.call("lines", c(list(x=x, y=observed + .05), observedpar))
    

    ## grid:
    do.call("abline", c(list(v = 1:(bars), h = rg[1]:rg[2] - 0.5), gridpar)) 

    if(plotenergy){
        ## separates two parts of the plot:
        do.call("abline", c(list(h = rg.s + 1.5), boxpar)) 
        
        ## calculate and plot energy:
        energy <- object@energy
        energy <- rg.s - 2 + 
            (3.5 * (energy - min(energy)) / diff(range(energy)))
        do.call("lines", c(list(x=x, y=energy), energypar))
        do.call("mtext", energylabel)
        do.call("axis", 
            c(list(at = rg.s + c(-2, 1.5), 
                label = round(range(object@energy), 1)), 
            axispar[["ax4"]]))
    }
        
    # clean up with a final box:
    do.call("box", boxpar)
}
