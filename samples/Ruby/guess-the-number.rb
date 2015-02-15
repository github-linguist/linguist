n = rand(1..10)
puts 'Guess the number: '
puts 'Wrong! Guess again: ' until gets.to_i == n
puts 'Well guessed!'
