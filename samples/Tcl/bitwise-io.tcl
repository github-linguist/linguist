package require Tcl 8.5

proc crunch {ascii} {
    binary scan $ascii B* bitstring
    # crunch: remove the extraneous leading 0 bit
    regsub -all {0(.{7})} $bitstring {\1} 7bitstring
    set padded "$7bitstring[string repeat 0 [expr {8 - [string length $7bitstring]%8}]]"
    return [binary format B* $padded]
}

proc expand {binary} {
    binary scan $binary B* padded
    # expand the 7 bit segments with their leading 0 bit
    set bitstring "0[join [regexp -inline -all {.{7}} $padded] 0]"
    return [binary format B* $bitstring]
}

proc crunch_and_write {ascii filename} {
    set fh [open $filename w]
    fconfigure $fh -translation binary
    puts -nonewline $fh [crunch $ascii]
    close $fh
}

proc read_and_expand {filename} {
    set fh [open $filename r]
    fconfigure $fh -translation binary
    set input [read $fh [file size $filename]]
    close $fh
    return [expand $input]
}

set original "This is an ascii string that will be crunched, written, read and expanded."
puts "my ascii string is [string length $original] bytes"

set filename crunched.out
crunch_and_write $original $filename

set filesize [file size $filename]
puts "the file containing the crunched text is $filesize bytes"

set expanded [read_and_expand $filename]

if {$original eq $expanded} {
    puts "the expanded string is the same as the original"
} else {
    error "not the same"
}
