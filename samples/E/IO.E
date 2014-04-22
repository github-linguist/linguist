# E sample from
# http://wiki.erights.org/wiki/Walnut/Ordinary_Programming/InputOutput
#File objects for hardwired files:
def file1 := <file:myFile.txt>
def file2 := <file:/home/marcs/myFile.txt>

#Using a variable for a file name:
def filePath := "c:\\docs\\myFile.txt"
def file3 := <file>[filePath]

#Using a single character to specify a Windows drive
def file4 := <file:c:/docs/myFile.txt>
def file5 := <c:/docs/myFile.txt>
def file6 := <c:\docs\myFile.txt>
