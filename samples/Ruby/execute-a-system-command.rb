string = `ls`
# runs command and returns its STDOUT as a string
string = %x{ls}
# ditto, alternative syntax

system "ls"
# runs command and returns its exit status; its STDOUT gets output to our STDOUT

print `ls`
#The same, but with back quotes

exec "ls"
# replace current process with another

# call system command and read output asynchronously
io = IO.popen('ls')
# ... later
io.each {|line| puts line}
