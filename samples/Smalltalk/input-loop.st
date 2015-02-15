|f|
f := FileStream open: 'afile.txt' mode: FileStream read.
[ f atEnd ] whileFalse: [ (f nextLine) displayNl ] .
