set io [socket localhost 256]
puts -nonewline $io "hello socket world"
close $io
