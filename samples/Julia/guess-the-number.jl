function guess()
  number = rand(1:10)
  print("Guess my number! ")
  while(chomp(readline(STDIN)) != string(number))
    print("Nope, try again! ")
  end
  println("Well guessed!")
end
