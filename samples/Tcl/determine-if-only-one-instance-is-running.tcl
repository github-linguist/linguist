package require Tcl 8.6
try {
    # Pick a port number based on the name of the main script executing
    socket -server {apply {{chan args} {close $chan}}} -myaddr localhost \
            [expr {1024 + [zlib crc32 [file normalize $::argv0]] % 30000}]
} trap {POSIX EADDRINUSE} {} {
    # Generate a nice error message
    puts stderr "Application $::argv0 already running?"
    exit 1
}
