set from 1
set to 10
set target [expr {int(rand()*($to-$from+1) + $from)}]
puts "I have thought of a number from $from to $to."
puts "Try to guess it!"
while 1 {
    puts -nonewline "Enter your guess: "
    flush stdout
    gets stdin guess
    if {![string is int -strict $guess] || $guess < $from || $guess > $to} {
	puts "Your guess should be an integer from $from to $to (inclusive)."
    } elseif {$guess > $target} {
	puts "Your guess was too high. Try again!"
    } elseif {$guess < $target} {
	puts "Your guess was too low. Try again!"
    } else {
	puts "Well done! You guessed it."
	break
    }
}
