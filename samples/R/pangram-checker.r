checkPangram <- function(sentence){
  my.letters <- tolower(unlist(strsplit(sentence, "")))
  is.pangram <- all(letters %in% my.letters)

  if (is.pangram){
    cat("\"", sentence, "\" is a pangram! \n", sep="")
  } else {
    cat("\"", sentence, "\" is not a pangram! \n", sep="")
  }
}
