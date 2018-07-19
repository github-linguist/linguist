f <- file("output.txt", "w")
close(f)

# it may fails and the exact syntax to achieve the root
# changes according to the operating system
f <- file("/output.txt", "w")
close(f)

success <- dir.create("docs")
success <- dir.create("/docs")
