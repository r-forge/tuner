bark2hz <- function(z){
    # Hynek's formula (taken from rasta/audspec.c)
    600 * sinh(z/6)
}

