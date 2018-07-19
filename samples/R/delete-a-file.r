file.remove("input.txt")
file.remove("/input.txt")

# or
file.remove("input.txt", "/input.txt")

# or
unlink("input.txt"); unlink("/input.txt")

# directories needs the recursive flag
unlink("docs", recursive = TRUE)
unlink("/docs", recursive = TRUE)
