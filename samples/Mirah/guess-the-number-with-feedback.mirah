def getInput:int
  s = System.console.readLine()
  Integer.parseInt(s)
end

number = int(Math.random() * 10 + 1)

puts "Guess the number between 1 and 10"

guessed = false

while !guessed do
  begin
    userNumber = getInput
    if userNumber == number
      guessed = true
      puts "You guessed it."
    elsif userNumber > number
      puts "Too high."
    else
      puts "Too low."
    end
  rescue NumberFormatException => e
    puts "Please enter an integer."
  end
end
