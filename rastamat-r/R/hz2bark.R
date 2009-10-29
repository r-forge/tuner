hz2bark <- function(f){
    # Inverse of Hynek's formula (see bark2hz)
    6 * asinh(f/600)
}

