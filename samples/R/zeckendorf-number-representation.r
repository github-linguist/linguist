zeckendorf <- function(number) {

  # Get an upper limit on Fibonacci numbers needed to cover number
  indexOfFibonacciNumber <- function(n) {
    if (n < 1) {
      2
    } else {
      Phi <- (1 + sqrt(5)) / 2
      invertClosedFormula <- log(n * sqrt(5)) / log(Phi)
      ceiling(invertClosedFormula)
    }
  }

  upperLimit <- indexOfFibonacciNumber(number)

  # Return the sequence as digits, sorted descending
  fibonacciSequenceDigits <- function(n) {
    fibGenerator <- function(f, ...) { c(f[2], sum(f)) }
    fibSeq <- Reduce(fibGenerator, 1:n, c(0,1), accumulate=TRUE)

    fibNums <- unlist(lapply(fibSeq, head, n=1))

    # drop last F0 and F1 and reverse sequence
    rev(fibNums[-2:-1])
  }

  digits <- fibonacciSequenceDigits(upperLimit)

  isInNumber <- function(digit) {
    if (number >= digit) {
      number <<- number - digit
      1
    } else {
      0
    }
  }

  zeckSeq <- Map(isInNumber, digits)

  # drop leading 0 and convert to String
  gsub("^0+1", "1", paste(zeckSeq, collapse=""))
}

print(unlist(lapply(0:20, zeckendorf)))
