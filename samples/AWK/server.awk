#!/usr/bin/gawk -f
BEGIN {
    # In the web, "\r\n" is line separator, not "\n"
    ORS = "\r\n"
    # Slurp mode
    RS = ""
    while (1) {
        server = "/inet/tcp/8080/0/0"
        print "HTTP/1.1 200 OK" |& server
        print ""                |& server
        print "Hello, world!"   |& server
        close(server)
    } 
}
