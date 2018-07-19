package require sound
# Potentially also require driver support for particular formats

# Load some sounds in; this part can be slow
snack::sound s1
s1 read $soundFile1
snack::sound s2
s2 read $soundFile2

# Play a sound for a while (0.1 seconds; this is a low-latency operation)
s1 play
after 100 set done 1; vwait done; # Run the event loop for a while
s1 stop

# Play two sounds at once (for 30 seconds) while mixing together
s1 play
s2 play
after 30000 set done 1; vwait done
s1 stop
s2 stop
