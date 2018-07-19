package require http
package require tls
http::register https 443 ::tls::socket

# Generate the authentication
set user theUser
set pass thePassword
dict set auth Authenticate "Basic [binary encode base64 ${user}:${pass}]"

# Make a secure authenticated connection
set token [http::geturl https://secure.example.com/ -headers $auth]

# Now as for conventional use of the “http” package
set data [http::data $token]
http::cleanup $token
