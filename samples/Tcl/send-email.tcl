package require smtp
package require mime
package require tls

set gmailUser *******
set gmailPass hunter2; # Hello, bash.org!

proc send_simple_message {recipient subject body} {
    global gmailUser gmailPass

    # Build the message
    set token [mime::initialize -canonical text/plain -string $body]
    mime::setheader $token Subject $subject

    # Send it!
    smtp::sendmessage $token -userame $gamilUser -password $gmailPass \
            -recipients $recipient -servers smtp.gmail.com -ports 587

    # Clean up
    mime::finalize $token
}

send_simple_message recipient@example.com "Testing" "This is a test message."
