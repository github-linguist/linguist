package require Thread

# Define the input thread
set input [thread::create {
    proc readFile {filename receiver} {
	set f [open $filename]
	while {[gets $f line] >= 0} {
	    thread::send $receiver [list line $line]
	}
	close $f
	thread::send $receiver lineCount lines
	puts "got $lines lines"
    }
    thread::wait
}]
# Define the output thread
set output [thread::create {
    set lines 0
    proc line {string} {
	puts $string
	incr ::lines
    }
    proc lineCount {} {return $::lines}
    thread::wait
}]

# Connect everything together and start the processing
thread::send $input [list readFile "input.txt" $output]
