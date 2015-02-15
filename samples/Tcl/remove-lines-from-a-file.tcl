proc removeLines {fileName startLine count} {
    # Work out range to remove
    set from [expr {$startLine - 1}]
    set to [expr {$startLine + $count - 2}]
    # Read the lines
    set f [open $fileName]
    set lines [split [read $f] "\n"]
    close $f
    # Write the lines back out, without removed range
    set f [open $fileName w]
    puts -nonewline $f [join [lreplace $lines $from $to] "\n"]
    close $f
}
