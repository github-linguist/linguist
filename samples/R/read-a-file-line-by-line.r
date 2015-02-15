conn <- file("notes.txt", "r")
while(length(line <- readLines(conn, 1)) > 0) {
    cat(line, "\n")
}
