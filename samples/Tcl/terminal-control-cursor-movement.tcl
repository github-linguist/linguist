# Simplification wrapper for when we're actually affecting the terminal
proc tput args {
    exec tput {*}$args >@stdout <@stdin
}

tput cub1;                 # one position to the left
tput cuf1;                 # one position to the right
tput cuu1;                 # up one line
tput cud1;                 # down one line
tput cr;                   # beginning of line
tput home;                 # top left corner

# For line ends and bottom, we need to determine size of terminal
set width [exec tput cols]
set height [exec tput lines]

tput hpa $width;	   # end of line
tput cpu $height $width;   # bottom right corner
