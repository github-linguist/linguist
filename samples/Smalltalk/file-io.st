| in out |
in := FileStream open: 'input.txt' mode: FileStream read.
out := FileStream open: 'output.txt' mode: FileStream write.
[ in atEnd ]
  whileFalse: [
     out nextPut: (in next)
  ]
