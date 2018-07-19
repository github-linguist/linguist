set.seed(15797, kind="Mersenne-Twister")

maxgenerations = 10
cellcount = 20
offendvalue = FALSE

## Cells are alive if TRUE, dead if FALSE
universe <- c(offendvalue,
              sample( c(TRUE, FALSE), cellcount, replace=TRUE),
              offendvalue)

## List of patterns in which the cell stays alive
stayingAlive <- lapply(list(c(1,1,0),
                            c(1,0,1),
                            c(0,1,0)), as.logical)

## x : length 3 logical vector
## map: list of length 3 logical vectors that map to patterns
##      in which x stays alive
deadOrAlive <- function(x, map) list(x) %in% map

cellularAutomata <- function(x, map) {
    c(x[1], apply(embed(x, 3), 1, deadOrAlive, map=map), x[length(x)])
}

deadOrAlive2string <- function(x) {
    paste(ifelse(x, '#', '_'), collapse="")
}

for (i in 1:maxgenerations) {
    universe <- cellularAutomata(universe, stayingAlive)
    cat(format(i, width=3), deadOrAlive2string(universe), "\n")
}
