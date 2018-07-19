set.seed(1234, kind="Mersenne-Twister")

## Easier if the string is a character vector
target <- unlist(strsplit("METHINKS IT IS LIKE A WEASEL", ""))

charset <- c(LETTERS, " ")
parent <- sample(charset, length(target), replace=TRUE)

mutaterate <- 0.01

## Number of offspring in each generation
C <- 100

## Hamming distance between strings normalized by string length is used
## as the fitness function.
fitness <- function(parent, target) {
    sum(parent == target) / length(target)
}

mutate <- function(parent, rate, charset) {
    p <- runif(length(parent))
    nMutants <- sum(p < rate)
    if (nMutants) {
        parent[ p < rate ] <- sample(charset, nMutants, replace=TRUE)
    }
    parent
}

evolve <- function(parent, mutate, fitness, C, mutaterate, charset) {
    children <- replicate(C, mutate(parent, mutaterate, charset),
                          simplify=FALSE)
    children <- c(list(parent), children)
    children[[which.max(sapply(children, fitness, target=target))]]
}

.printGen <- function(parent, target, gen) {
    cat(format(i, width=3),
        formatC(fitness(parent, target), digits=2, format="f"),
        paste(parent, collapse=""), "\n")
}

i <- 0
.printGen(parent, target, i)
while ( ! all(parent == target)) {
    i <- i + 1
    parent <- evolve(parent, mutate, fitness, C, mutaterate, charset)

    if (i %% 20 == 0) {
        .printGen(parent, target, i)
    }
}
.printGen(parent, target, i)
