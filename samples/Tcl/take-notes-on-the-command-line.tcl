# Make it easier to change the name of the notes file
set notefile notes.txt

if {$argc} {
    # Write a message to the file
    set msg [clock format [clock seconds]]\n\t[join $argv " "]
    set f [open $notefile a]
    puts $f $msg
    close $f
} else {
    # Print the contents of the file
    catch {
	set f [open $notefile]
	fcopy $f stdout
	close $f
    }
}
