## stolen from e1071:
hamming.window <- function (n) 
{
    if (n == 1) 
        c <- 1
    else {
        n <- n - 1
        c <- 0.54 - 0.46 * cos(2 * pi * (0:n)/n)
    }
    return(c)
}
hanning.window <- function (n) 
{
    if (n == 1) 
        c <- 1
    else {
        n <- n - 1
        c <- 0.5 - 0.5 * cos(2 * pi * (0:n)/n)
    }
    return(c)
}
