proc letterHistogram {fileName} {
    # Initialize table (in case of short texts without every letter)
    for {set i 97} {$i<=122} {incr i} {
        set frequency([format %c $i]) 0
    }
    # Iterate over characters in file
    set f [open $fileName]
    foreach c [split [read $f] ""] {
        # Count them if they're alphabetic
        if {[string is alpha $c]} {
            incr frequency([string tolower $c])
        }
    }
    close $f
    # Print the histogram
    parray frequency
}

letterHistogram the/sample.txt
