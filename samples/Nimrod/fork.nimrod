import posix

var pid = fork()
if pid < 0:
    # error forking a child
elif pid > 0:
    # parent, and pid is process id of child
else:
    # child
    quit()
# further Parent stuff here
