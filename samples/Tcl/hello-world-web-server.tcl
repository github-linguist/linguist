proc accept {chan addr port} {
    while {[gets $chan] ne ""} {}
    puts $chan "HTTP/1.1 200 OK\nConnection: close\nContent-Type: text/plain\n"
    puts $chan "Goodbye, World!"
    close $chan
}
socket -server accept 8080
vwait forever
