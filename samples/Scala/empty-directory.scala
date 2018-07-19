import java.io.File

def isDirEmpty(file:File) : Boolean =
   return file.exists && file.isDirectory && file.list.isEmpty
