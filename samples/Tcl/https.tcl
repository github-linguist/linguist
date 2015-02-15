package require http
package require tls

# Tell the http package what to do with “https:” URLs.
#
# First argument is the protocol name, second the default port, and
# third the connection builder command
http::register "https" 443 ::tls::socket

# Make a secure connection, which is almost identical to normal
# connections except for the different protocol in the URL.
set token [http::geturl "https://sourceforge.net/"]

# Now as for conventional use of the “http” package
puts [http::data $token]
http::cleanup $token
