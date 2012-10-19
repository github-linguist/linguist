#!/usr/bin/awk -f
BEGIN {
    print "Guess the number from 1 to 100"
    srand()
    number = int(rand() * 100 + 1)
}
{
    $0 = int($0)
}
$0 <= 0 || $0 > 100 {
    print "Invalid guess!"
    next
} 
$0 == number {
    print "You got correct number! It took you", NR, "guesses!"
    exit
}
$0 < number {
    print "Your guess is too low!"
}
$0 > number {
    print "Your guess is too high!"
}
