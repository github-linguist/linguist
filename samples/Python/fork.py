import os

pid = os.fork()
if pid > 0:
 # parent code
else:
 # child code
