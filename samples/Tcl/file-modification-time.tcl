# Get the modification time:
set timestamp [file mtime $filename]

# Set the modification time to ‘now’:
file mtime $filename [clock seconds]
