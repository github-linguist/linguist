f <- function() {
    print("Guess a number between 1 and 10 until you get it right.")
    n <- 1 + floor(10*runif(1))
    while (as.numeric(readline()) != n) {
        print("Try again.")
    }
    print("You got it!")
}
