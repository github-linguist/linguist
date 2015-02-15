# Open a file for writing, and truncate it to 1234 bytes.
File.open("file", "ab") { |f| f.truncate(1234) }

# Truncate a file to 567 bytes.
File.truncate("file", 567)
