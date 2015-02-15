proc main {} {
    fconfigure stdout -buffering none
    set length 4

    puts "I have chosen a number from $length unique digits from 1 to 9 arranged in a random order.
You need to input a $length digit, unique digit number as a guess at what I have chosen
    "

    while true {
        set word [generateWord $length]
        set count 1
        while {[set guess [getGuess $length]] ne $word} {
            printScore $length $word $guess
            incr count
        }
        puts "You guessed correctly in $count tries."
        if {[yn "Play again?"] eq "n"} break
    }
}

proc generateWord {length} {
    set chars 123456789
    for {set i 1} {$i <= $length} {incr i} {
        set idx [expr {int(rand() * [string length $chars])}]
        append word [string index $chars $idx]
        set chars [string replace $chars $idx $idx]
    }
    return $word

    # here's another way to generate word with no duplications
    set word ""
    while {[string length $word] < $length} {
        set char [expr {int(1 + 9*rand())}]
        if {[string first $char $word] == -1} {
            append word $char
        }
    }
}

proc getGuess {length} {
    puts -nonewline "Enter your guess: "
    while true {
        gets stdin guess
        if {[string match [string repeat {[1-9]} $length] $guess]} {
            return $guess
        }
        if {[string tolower [string trim $guess]] eq "quit"} {
            puts Bye
            exit
        }
        puts "The word must be $length digits between 1 and 9 inclusive.  Try again."
    }
}

proc printScore {length word guess} {
    set bulls 0
    set cows 0
    for {set i 0} {$i < $length} {incr i} {
        if {[string index $word $i] eq [string index $guess $i]} {
            incr bulls
            set word [string replace $word $i $i +]
        }
    }
    puts "  $bulls bulls"
    for {set i 0} {$i < $length} {incr i} {
        if {[set j [string first [string index $guess $i] $word]] != -1} {
            incr cows
            set word [string replace $word $j $j -]
        }
    }
    puts "  $cows cows"
}

proc yn {msg} {
    while true {
        puts -nonewline "$msg \[y/n] "
        gets stdin ans
        set char [string tolower [string index [string trim $ans] 0]]
        if {$char eq "y" || $char eq "n"} {
            return $char
        }
    }
}

main
