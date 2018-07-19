# Read in data, discard useless bits
dfr <- read.table("mlijobs.txt")
dfr <- dfr[,c(2,4)]
# Find most concurrent licences, and when
n.checked.out <- cumsum(ifelse(dfr$V2=="OUT", 1, -1))
times <- strptime(dfr$V4, "%Y/%m/%d_%H:%M:%S")
most.checked.out <- max(n.checked.out)
when.most.checked.out <- times[which(n.checked.out==most.checked.out)]
# As a bonus, plot license use
plot(times, n.checked.out, type="s")
