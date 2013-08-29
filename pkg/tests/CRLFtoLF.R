# Uwe: setwd("D:/uwe/R/library-devel/tuneR/pkg/tests")
d <- dir()
d <- setdiff(d, c("Testfiles", "CRLFtoLF.R"))
for(i in d){
    x <- readLines(i)
    con <- file(i, open="wb")
    writeLines(x, con=con, sep="\n")
    close(con)
}
