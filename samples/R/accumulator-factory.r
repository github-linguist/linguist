accumulatorFactory <- function(init) {
  currentSum <- init
  function(add) {
    currentSum <<- currentSum + add
    currentSum
  }
}
