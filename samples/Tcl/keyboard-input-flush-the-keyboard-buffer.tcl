# No waiting for input
fconfigure stdin -blocking 0
# Drain the data by not saving it anywhere
read stdin

# Flip back into blocking mode (if necessary)
fconfigure stdin -blocking 1
