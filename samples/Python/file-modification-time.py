import os

#Get modification time:
modtime = os.path.getmtime('filename')

#Set the access and modification times:
os.utime('path', (actime, mtime))

#Set just the modification time:
os.utime('path', (os.path.getatime('path'), mtime))

#Set the access and modification times to the current time:
os.utime('path', None)
