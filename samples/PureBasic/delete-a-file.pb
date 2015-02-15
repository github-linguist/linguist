DeleteFile("input.txt")
DeleteDirectory("docs","")  ; needs to delete all included files
DeleteFile("/input.txt")
DeleteDirectory("/docs","*.*")  ; deletes all files according to a pattern

DeleteDirectory("/docs","",#PB_FileSystem_Recursive)  ; deletes all files and directories recursive
