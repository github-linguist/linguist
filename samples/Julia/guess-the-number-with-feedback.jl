function guess_feedback(n)
  number = rand(1:n)
  print("I choose a number between 1 and $n\nYour guess? ")
  while((guess = chomp(readline(STDIN))) != string(number))
    isdigit(guess) ?
      print("Too $(int(guess) < number ? "small" : "big")\nNew guess?  ") :
      print("Enter an integer please\nNew guess?  ")
  end
  print("you guessed right!")
end
