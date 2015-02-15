halve <- function(a) floor(a/2)
double <- function(a) a*2
iseven <- function(a) (a%%2)==0

ethiopicmult <- function(plier, plicand, tutor=FALSE) {
  if (tutor) { cat("ethiopic multiplication of", plier, "and", plicand, "\n") }
  result <- 0
  while(plier >= 1) {
    if (!iseven(plier)) { result <- result + plicand }
    if (tutor) {
      cat(plier, ", ", plicand, " ", ifelse(iseven(plier), "struck", "kept"), "\n", sep="")
    }
    plier <- halve(plier)
    plicand <- double(plicand)
  }
  result
}

print(ethiopicmult(17, 34, TRUE))
