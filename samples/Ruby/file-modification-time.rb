#Get modification time:
modtime = File.mtime('filename')

#Set the access and modification times:
File.utime(actime, mtime, 'path')

#Set just the modification time:
File.utime(File.atime('path'), mtime, 'path')

#Set the access and modification times to the current time:
File.utime(nil, nil, 'path')
