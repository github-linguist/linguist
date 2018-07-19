number = rand(1..10)

puts "Guess the number between 1 and 10"

loop do
  begin
    user_number = Integer(gets)
    if user_number == number
      puts "You guessed it."
      break
    elsif user_number > number
      puts "Too high."
    else
      puts "Too low."
    end
  rescue ArgumentError
    puts "Please enter an integer."
  end
end
