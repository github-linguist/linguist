# A helper to make code more readable
proc terminal {args} {
    exec /usr/bin/tput {*}$args >/dev/tty
}

# Save the screen with the "enter_ca_mode" capability, a.k.a. 'smcup'
terminal smcup
# Some indication to users what is happening...
puts "This is the top of a blank screen. Press Return/Enter to continue..."
gets stdin
# Restore the screen with the "exit_ca_mode" capability, a.k.a. 'rmcup'
terminal rmcup
