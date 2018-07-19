set target [expr {int(rand()*10 + 1)}]
puts "I have thought of a number."
puts "Try to guess it!"
while 1 {
    puts -nonewline "Enter your guess: "
    flush stdout
    gets stdin guess
    if {$guess == $target} {
	break
    }
    puts "Your guess was wrong. Try again!"
}
puts "Well done! You guessed it."
