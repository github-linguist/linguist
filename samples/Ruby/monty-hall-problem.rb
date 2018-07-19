n = 10_000                  #number of times to play

stay = switch = 0           #sum of each strategy's wins

n.times do                  #play the game n times

  #the doors reveal 2 goats and a car
  doors = [ :goat, :goat, :car ].shuffle

  #random guess
  guess = rand(3)

  #random door shown, but it is neither the guess nor the car
  begin shown = rand(3) end while shown == guess || doors[shown] == :car

  if doors[guess] == :car
    #staying with the initial guess wins if the initial guess is the car
    stay += 1
  else
    #switching guesses wins if the unshown door is the car
    switch += 1
  end

end

puts "Staying wins %.2f%% of the time."   % (100.0 * stay   / n)
puts "Switching wins %.2f%% of the time." % (100.0 * switch / n)
