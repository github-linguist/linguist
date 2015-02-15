target <- sample(1:9,4)
bulls <- 0
cows <- 0
attempts <- 0
while (bulls != 4)
  {
  input <- readline("Guess a 4-digit number with no duplicate digits or 0s: ")
  if (nchar(input) == 4)
    {
    input <- as.integer(strsplit(input,"")[[1]])
    if ((sum(is.na(input)+sum(input==0))>=1) | (length(table(input)) != 4)) {print("Malformed input!")} else {
      bulls <- sum(input == target)
      cows <- sum(input %in% target)-bulls
      cat("\n",bulls," Bull(s) and ",cows, " Cow(s)\n")
      attempts <- attempts + 1
      }
    } else {print("Malformed input!")}
  }
print(paste("You won in",attempts,"attempt(s)!"))
