# Get the value
file.info(filename)$mtime

#To set the value, we need to rely on shell commands.  The following works under windows.
shell("copy /b /v filename +,,>nul")
# and on Unix (untested)
shell("touch -m filename")
