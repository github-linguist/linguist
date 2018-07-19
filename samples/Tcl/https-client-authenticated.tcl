package require http
package require tls

set cert myCert.p12
http::register https 443 [list \
    ::tls::socket -certfile $cert -password getPass]
proc getPass {} {
    return "myPassword";  # Just a noddy example...
}

# Make a secure authenticated connection
set token [http::geturl https://verysecure.example.com/]

# Now as for conventional use of the “http” package
set data [http::data $token]
http::cleanup $token
