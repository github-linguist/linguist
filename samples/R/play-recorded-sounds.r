#Setup
# Warning: The example files are Windows specific.
library(sound)
media_dir <- file.path(Sys.getenv("SYSTEMROOT"), "Media")
chimes <- loadSample(file.path(media_dir, "chimes.wav"))
chord <- loadSample(file.path(media_dir, "chord.wav"))

# Play sequentially
play(appendSample(chimes, chord))

# Play simultaneously
play(chimes + chord)

# Stopping before the end
play(cutSample(chimes, 0, 0.2))

#Looping
for(i in 1:3) play(chimes)                 #leaves a gap between instances

three_chimes <- lapply(vector(3, mode = "list"), function(x) chimes)
play(do.call(appendSample, three_chimes))  #no gap, see also cutSampleEnds

# Volume control
play(3 * chimes)

#Stereo effect
play(stereo(chimes, chord))

#Other actions (not obviously possible)
